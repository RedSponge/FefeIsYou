use bevy::{
    ecs::system::EntityCommand,
    prelude::*,
    utils::{HashMap, HashSet},
};

const TILE_SIZE: f32 = 32.0;

#[derive(Component)]
struct Board {
    size: IVec2,
}

#[derive(Component)]
struct Push;

#[derive(Component)]
struct Stop;

#[derive(Component)]
struct You;

#[derive(Component, Hash, Copy, Eq, PartialEq, Clone)]
struct TilePosition {
    pos: IVec2,
}

#[derive(Component, Default)]
struct Intention {
    direction: IVec2,
}

#[derive(Component)]
struct TileData {
    tile_type: TileType,
}

impl TileData {
    fn from_actor(actor: Actor) -> Self {
        Self {
            tile_type: TileType::Actor(actor),
        }
    }

    fn from_actor_text(actor: Actor) -> Self {
        Self {
            tile_type: TileType::Text(Text::Actor(actor)),
        }
    }

    fn from_operation(operation: Operation) -> Self {
        Self {
            tile_type: TileType::Text(Text::Operation(operation)),
        }
    }

    fn from_effect(effect: Effect) -> Self {
        Self {
            tile_type: TileType::Text(Text::Effect(effect)),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum TileType {
    Actor(Actor),
    Text(Text),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Actor {
    Fefe,
    Rock,
    Wall,
}

#[derive(Component, PartialEq, Eq, Debug)]
enum Text {
    Actor(Actor),
    Operation(Operation),
    Effect(Effect),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Operation {
    Is,
}
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Effect {
    You,
    Push,
    Stop,
}

fn add_effect_to_entity(commands: &mut Commands, entity: Entity, effect: &Effect) {
    let mut entity = commands.get_entity(entity).unwrap();
    match effect {
        Effect::You => entity.insert((You,)),
        Effect::Stop => entity.insert((Stop,)),
        Effect::Push => entity.insert((Push,)),
    };
}

#[derive(Debug)]

enum RuleEffect {
    Effects(Vec<Effect>),
    Transformation(Actor),
}

#[derive(Debug)]
struct Rule {
    affected: Vec<TileType>,
    operation: Operation,
    effect: RuleEffect,
}

#[derive(Resource)]
struct Rules {
    rules: Vec<Rule>,
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, (spawn_camera,))
        .add_systems(Startup, (add_rules_resource, spawn_board, spawn_map))
        .add_systems(
            Update,
            (
                detect_rules,
                apply_rules,
                movement,
                compute_push_chains,
                move_by_intentions,
                move_to_tile_position,
                apply_texture,
            )
                .chain(),
        )
        .run();
}

fn add_rules_resource(mut commands: Commands) {
    commands.insert_resource(Rules {
        rules: vec![
            Rule {
                affected: vec![TileType::Actor(Actor::Fefe)],
                operation: Operation::Is,
                effect: RuleEffect::Effects(vec![Effect::You]),
            },
            Rule {
                affected: vec![TileType::Actor(Actor::Rock)],
                operation: Operation::Is,
                effect: RuleEffect::Effects(vec![Effect::Push]),
            },
            Rule {
                affected: vec![TileType::Actor(Actor::Wall)],
                operation: Operation::Is,
                effect: RuleEffect::Effects(vec![Effect::Stop]),
            },
        ],
    })
}

fn clear_effects(entity: Entity, commands: &mut Commands) {
    commands.get_entity(entity).unwrap().remove::<You>();
    commands.get_entity(entity).unwrap().remove::<Stop>();
    commands.get_entity(entity).unwrap().remove::<Push>();
}

fn apply_rule(commands: &mut Commands, rule: &Rule, entities: &mut Query<(Entity, &mut TileData)>) {
    let mut affected: Vec<_> = entities
        .iter_mut()
        .filter(|(_, data)| rule.affected.contains(&data.tile_type))
        .collect();
    match rule.operation {
        Operation::Is => match &rule.effect {
            RuleEffect::Effects(effects) => {
                affected.iter().for_each(|(e, _)| {
                    effects
                        .iter()
                        .for_each(|effect| add_effect_to_entity(commands, *e, effect));
                });
            }
            RuleEffect::Transformation(actor) => affected.iter_mut().for_each(|(_, data)| {
                data.tile_type = TileType::Actor(*actor);
            }),
        },
    }
}

fn apply_rules(
    mut commands: Commands,
    rules: Res<Rules>,
    mut entities: Query<(Entity, &mut TileData)>,
) {
    for (entity, data) in &entities {
        clear_effects(entity, &mut commands);
        if let TileType::Text(_) = &data.tile_type {
            commands.get_entity(entity).unwrap().insert(Push);
        }
    }

    for rule in &rules.rules {
        apply_rule(&mut commands, rule, &mut entities)
    }
}

fn spawn_camera(mut commands: Commands) {
    commands.spawn((Camera2dBundle::default(),));
}

fn spawn_board(mut commands: Commands, assets: Res<AssetServer>) {
    let board = Board {
        size: IVec2::new(30, 30),
    };
    let physical_size = Vec2::new(board.size.x as f32, board.size.y as f32) * TILE_SIZE;

    commands
        .spawn((SpriteBundle {
            sprite: Sprite {
                custom_size: Some(physical_size),
                color: Color::GRAY,
                ..default()
            },
            texture: assets.load("pixel.png"),
            transform: Transform::from_xyz(physical_size.x / 2.0, physical_size.y / 2.0, -5.0),
            ..default()
        },))
        .insert(board);
}

fn spawn_map(mut commands: Commands) {
    commands.spawn(TileBundle::new(
        IVec2::new(0, 0),
        TileData::from_actor(Actor::Fefe),
    ));
    commands.spawn(TileBundle::new(
        IVec2::new(2, 2),
        TileData::from_actor(Actor::Rock),
    ));
    commands.spawn(TileBundle::new(
        IVec2::new(3, 3),
        TileData::from_actor(Actor::Rock),
    ));
    commands.spawn(TileBundle::new(
        IVec2::new(4, 4),
        TileData::from_actor(Actor::Wall),
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(1, 5), TileData::from_actor_text(Actor::Fefe)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(2, 5), TileData::from_operation(Operation::Is)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(3, 5), TileData::from_effect(Effect::You)),
        Push,
    ));

    commands.spawn((
        TileBundle::new(IVec2::new(1, 6), TileData::from_actor_text(Actor::Rock)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(2, 6), TileData::from_operation(Operation::Is)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(3, 6), TileData::from_effect(Effect::Push)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(1, 7), TileData::from_actor_text(Actor::Wall)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(2, 7), TileData::from_operation(Operation::Is)),
        Push,
    ));
    commands.spawn((
        TileBundle::new(IVec2::new(3, 7), TileData::from_effect(Effect::Stop)),
        Push,
    ));
}

#[derive(Bundle)]
struct TileBundle {
    pos: TilePosition,
    data: TileData,
    intention: Intention,
    sprite_bundle: SpriteBundle,
}

impl TileBundle {
    fn new(pos: IVec2, data: TileData) -> Self {
        let pos = TilePosition { pos };
        let intention = Intention::default();
        let sprite_bundle = SpriteBundle::default();
        Self {
            pos,
            data,
            intention,
            sprite_bundle,
        }
    }
}

fn movement(mut yous: Query<&mut Intention, With<You>>, input: Res<Input<KeyCode>>) {
    for mut you in yous.iter_mut() {
        if input.just_pressed(KeyCode::Left) {
            you.direction.x -= 1;
        }
        if input.just_pressed(KeyCode::Right) {
            you.direction.x += 1;
        }
        if input.just_pressed(KeyCode::Up) {
            you.direction.y += 1;
        }
        if input.just_pressed(KeyCode::Down) {
            you.direction.y -= 1;
        }
    }
}

fn compute_push_chains(
    mut movers: Query<(Entity, &TilePosition, &mut Intention)>,
    tiles: Query<(Entity, &TilePosition)>,
    pushables: Query<&Push>,
    stoppers: Query<&Stop>,
) {
    let mut position_to_entities: HashMap<IVec2, Vec<Entity>> = HashMap::new();
    for (entity, tile) in tiles.iter() {
        let tile = tile.pos;
        if !position_to_entities.contains_key(&tile) {
            position_to_entities.insert(tile, vec![]);
        }
        position_to_entities.get_mut(&tile).unwrap().push(entity);
    }
    let mut new_intentions: Vec<(Entity, IVec2)> = vec![];

    for (me, pos, intention) in movers.iter() {
        let mut target = Some(pos.pos + intention.direction);
        let mut entity_chain = vec![me];
        let mut new_direction = intention.direction;
        if intention.direction == IVec2::ZERO {
            continue;
        }
        'outer: while target.is_some() {
            if let Some(entities) = position_to_entities.get(&target.unwrap()) {
                // Find any stoppers
                for &entity in entities {
                    if entity == me {
                        continue;
                    }
                    if stoppers.get(entity).is_ok() && !pushables.get(entity).is_ok() {
                        new_direction = IVec2::ZERO;
                        break 'outer;
                    }
                }
                let old_chain_len = entity_chain.len();
                for &entity in entities {
                    if entity == me {
                        continue;
                    }
                    if pushables.get(entity).is_ok() {
                        entity_chain.push(entity);
                    }
                }
                if old_chain_len != entity_chain.len() {
                    target = Some(target.unwrap() + intention.direction);
                } else {
                    target = None;
                }
            } else {
                target = None;
            }
        }
        for entity in entity_chain {
            new_intentions.push((entity, new_direction));
        }
    }

    for (entity, direction) in new_intentions {
        if let Ok((_e, _p, mut intention)) = movers.get_mut(entity) {
            intention.direction = direction;
        }
    }
}

fn move_to_tile_position(mut entities: Query<(&TilePosition, &mut Transform)>) {
    for (tile, mut transform) in entities.iter_mut() {
        transform.translation.x = tile.pos.x as f32 * TILE_SIZE + TILE_SIZE / 2.0;
        transform.translation.y = tile.pos.y as f32 * TILE_SIZE + TILE_SIZE / 2.0;
    }
}

fn move_by_intentions(mut entities: Query<(&mut Intention, &mut TilePosition)>) {
    for (mut intention, mut position) in &mut entities {
        position.pos += intention.direction;
        intention.direction = IVec2::ZERO;
    }
}

fn apply_texture(mut entities: Query<(&mut Handle<Image>, &TileData)>, assets: Res<AssetServer>) {
    for (mut image, data) in &mut entities {
        let texture_name = match &data.tile_type {
            TileType::Actor(actor) => match actor {
                Actor::Fefe => "actor/tile/fefe.png",
                Actor::Rock => "actor/tile/rock.png",
                Actor::Wall => "actor/tile/wall.png",
            },
            TileType::Text(text) => match text {
                Text::Actor(actor) => match actor {
                    Actor::Fefe => "actor/text/fefe.png",
                    Actor::Rock => "actor/text/rock.png",
                    Actor::Wall => "actor/text/wall.png",
                },
                Text::Operation(operation) => match operation {
                    Operation::Is => "operation/is.png",
                },
                Text::Effect(effect) => match effect {
                    Effect::You => "effect/you.png",
                    Effect::Push => "effect/push.png",
                    Effect::Stop => "effect/stop.png",
                },
            },
        };
        *image = assets.load(texture_name);
    }
}

fn detect_rules(entities: Query<(&TilePosition, &TileData)>, mut rules: ResMut<Rules>) {
    let mut position_to_entities: HashMap<IVec2, Vec<&TileData>> = HashMap::new();
    for (tile, data) in entities.iter() {
        let tile = tile.pos;
        if !position_to_entities.contains_key(&tile) {
            position_to_entities.insert(tile, vec![]);
        }
        position_to_entities.get_mut(&tile).unwrap().push(data);
    }

    let mut detected_rules: Vec<Rule> = vec![];
    for (tile, data) in entities.iter() {
        if let TileType::Text(Text::Actor(affected)) = data.tile_type {
            for direction in [IVec2::X, IVec2::NEG_Y] {
                let operation_pos = tile.pos + direction;
                let operation = position_to_entities.get(&operation_pos);
                if operation.is_none() {
                    continue;
                }

                let operation = operation
                    .unwrap()
                    .iter()
                    .filter_map(|d| {
                        if let TileType::Text(Text::Operation(op)) = d.tile_type {
                            Some(op)
                        } else {
                            None
                        }
                    })
                    .next();

                if operation.is_none() {
                    continue;
                }

                let operation = operation.unwrap();
                let effect_pos = operation_pos + direction;
                let effect = position_to_entities.get(&effect_pos);
                if effect.is_none() {
                    continue;
                }
                let effect = effect
                    .unwrap()
                    .iter()
                    .filter_map(|d| {
                        if let TileType::Text(Text::Effect(effect)) = d.tile_type {
                            Some(RuleEffect::Effects(vec![effect]))
                        } else if let TileType::Text(Text::Actor(actor)) = d.tile_type {
                            Some(RuleEffect::Transformation(actor))
                        } else {
                            None
                        }
                    })
                    .next();

                if effect.is_none() {
                    continue;
                }

                let effect = effect.unwrap();

                let rule = Rule {
                    affected: vec![TileType::Actor(affected)],
                    operation,
                    effect,
                };
                detected_rules.push(rule);
            }
        }
    }
    rules.rules = detected_rules;
}
