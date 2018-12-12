use std::env;
use std::collections::HashMap;

#[derive(Hash, PartialEq, Eq, Copy, Clone, Debug)]
struct Block {
    position: (u16, u16),
    size: (u16),
}

fn main() {
    let mut args = env::args();
    args.next();
    let serial: i64 = args.next().unwrap().parse().unwrap();
    let size: u16 = args.next().unwrap().parse().unwrap();

    let mut cache = HashMap::new();

    let res = blocks(size).into_iter()
        .map(|block| {
            (block, block_level_with_cache(&mut cache, serial, block))
        })
        .max_by_key(|&(_, l)| l).unwrap().0;

    print!("{:?}", res);
}

fn blocks(max_size: u16) -> Vec<Block> {
    let mut blocks = Vec::with_capacity((1..=(max_size as usize)).map(|x| x.pow(2)).sum());
    for x in 1..=max_size {
        for y in 1..=max_size {
            for size in 1..=(max_size - x + 1).min(max_size - y + 1) {
                blocks.push(Block { position: (x, y), size });
            }
        }
    }

    blocks
}

fn block_level_with_cache(cache: &mut HashMap<Block, i64>, serial: i64, block: Block) -> i64 {
    if let Some(level) = cache.get(&block) {
        *level
    } else {
        let pl =
            if block.size == 1 {
                power_level(serial, block.position)
            } else {
                split_block(block).into_iter()
                    .map(|block| block_level_with_cache(cache, serial, block))
                    .sum()
            };

        cache.insert(block, pl);
        pl
    }
}

fn power_level(serial: i64, (x, y): (u16, u16)) -> i64 {
    let rack_id = x as i64 + 10;
    hundreds((rack_id * (y as i64) + serial) * rack_id) - 5
}

fn hundreds(num: i64) -> i64 {
    (num / 100) % 10
}

fn split_block(Block { position: (x, y), size }: Block) -> Vec<Block> {
    if size == 1 {
        vec![Block { position: (x, y), size }]
    } else if size % 2 == 0 {
        let half_size = size / 2;
        vec![
            Block { position: (x, y), size: half_size },
            Block { position: (x + half_size, y), size: half_size },
            Block { position: (x, y + half_size), size: half_size },
            Block { position: (x + half_size, y + half_size), size: half_size },
        ]
    } else {
        let bottom = (x..=(x + size - 1)).map(|x| {
            Block { position: (x, y + size - 1), size: 1 }
        });

        let right = (y..=(y + size - 2)).map(|y| {
            Block { position: (x + size - 1, y), size: 1 }
        });

        let mut res = vec![Block { position: (x, y), size: size - 1 }];

        res.extend(right);
        res.extend(bottom);

        res
    }
}