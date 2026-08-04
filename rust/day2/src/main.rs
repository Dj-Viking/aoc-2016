use std::fs::{read_to_string};
use std::io;

//                      col,row
const LEFT:  (i8, i8) = (0 ,-1);
const RIGHT: (i8, i8) = (0 , 1);
const UP:    (i8, i8) = (-1, 0);
const DOWN:  (i8, i8) = (1 , 0);

fn main() {

	// part1
	let grid_file = read_to_string("./samplekeypad")
		.unwrap();

	let mut position: (usize, usize) = (1, 1);
// 	position.0 = 2;
// 	println!("{:?}", position);
// (2, 0)
// ()

	let instructions = read_to_string("./sampleinstructions")
		.unwrap();

	// todo: this needs to still be rows of letters for directions to the next number of the door
	// code 
	let dirs = instructions
		.split("\n")
		.filter(|x| !x.is_empty())
		.collect::<Vec<_>>();

	let mut grid = grid_file
		.split("\n")
		.filter(|x| !x.is_empty())
			.map(|l| l.split(" ")
					 .map(|x| x.to_string())
				     .collect::<Vec<_>>()
			)
		.collect::<Vec<_>>();


	// highlight starting point
	let grid_with_start = "[".to_string() + &grid[1][1] + "]";
	grid[1][1] = grid_with_start; 

	let mut input = String::new();

	// each line of directions indicates the number that the grid ends up on from the
	// starting point for each line of directions
	let mut passkey = String::new();

	for row in 0..dirs.len() {
		println!("dirs row is {:?}", dirs[row]);
		let chars = dirs[row].chars().collect::<Vec<_>>();
		for col in 0..chars.len() {
			let direction_to_match = String::new() + &chars[col].to_string();
			println!("direction is [{:?}]", direction_to_match);
			let current_direction = match direction_to_match.as_str() {
				"U" => { UP    },
				"D" => { DOWN  },
				"L" => { LEFT  },
				"R" => { RIGHT },
				_   => { panic!("unreachable reached...invalid direction");}
			};

			println!("current direction: {:?}", current_direction);

			// reset current position
			grid[position.0][position.1] = grid[position.0][position.1].replace("[", "").replace("]", "");

			// update current position according to current direction
			position.0 = {
				if ((position.0 as i8) + current_direction.0) < 0 || ((position.0 as i8) + current_direction.0) > ((grid.len() as i8) - 1)
				{
					position.0
				} else {
					<i8 as TryInto<usize>>::try_into((position.0 as i8) + current_direction.0).unwrap() as usize
				}
			};
			position.1 = {
				if ((position.1 as i8) + current_direction.1 < 0) || ((position.1 as i8) + current_direction.1) > ((grid[0].len() as i8) - 1) 
				{
					position.1
				} else {
					<i8 as TryInto<usize>>::try_into((position.1 as i8) + current_direction.1).unwrap() as usize
				}
			};

			grid[position.0][position.1] = "[".to_string() + &grid[position.0][position.1] + "]";
			
			for i in 0..grid.len() {
				println!("{:?}",grid[i]);
			}
			
			//todo: if on the last instruction of the instruction row
			// then update the passkey to the number that was last arrived to for the last
			// instruction

			// wait for enter key
			println!("press enter to continue");
			match io::stdin().read_line(&mut input) {
				Ok(n)  => { println!("================================================================"); },
				Err(e) => { println!("error reading input: {:?}", e)}
			}
		}
	}
	// part 1

}

