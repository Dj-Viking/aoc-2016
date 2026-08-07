use std::fs;
// find all triangle side length combos that are possible
fn main() {
	part1();
	part2();
}

fn part1() {

	let sides_list = fs::read_to_string("./input").unwrap();

	// sum of any two sides must be larger than the remaining side
	// 5 10 25 impossible because 
	// 5 + 10  (15) is NOT larger than 25
	// 10 + 25 (35) is     larger than 5
	// 5 + 25  (30) is     larger than 10
	// because one of the sums does not end up being larger than the remaining then impossible
	//
	let list = sides_list
		.split("\n")
		.filter(|line| !line.is_empty())
		.map(|line| line.split(" ")
				    .filter(|item| !item.is_empty())
					.map(|string| string.parse::<u32>().unwrap())
				    .collect::<Vec<_>>()
		)
		.collect::<Vec<_>>();

	let mut possible_count = 0;

	for i in 0..list.len() {
		if is_possible(&list[i]) {
			possible_count += 1;
		}
	}

	println!("part1: {}", possible_count);
}

fn is_possible(sides: &Vec<u32>) -> bool {
	let mut count = 0;

	if sides[0] + &sides[1] > sides[2] { count += 1; }
	if sides[1] + &sides[2] > sides[0] { count += 1; }
	if sides[0] + &sides[2] > sides[1] { count += 1; }

	count == 3
}

// triangles are groups of 3 vertically
// sum of any two sides must be larger than the remaining side
// 5 10 25 impossible because 
// 5 + 10  (15) is NOT larger than 25
// 10 + 25 (35) is     larger than 5
// 5 + 25  (30) is     larger than 10
// because one of the sums does not end up being larger than the remaining then impossible

// should I parse the current list into separate vertical column lists
// and then loop through each vertical list until it's empty 
// removing numbers that i've already checked for side-length-possibility
// before going to the next
// vertical list
fn part2() {
	let sides_list = fs::read_to_string("./input").unwrap();

	let list = sides_list 
		.split("\n")
		.filter(|line| !line.is_empty())
		.map(|line| line.trim()
		            .split(" ")
					.filter(|x| !x.is_empty())
				    .collect::<Vec<_>>()
	    )
		.flatten()
		.collect::<Vec<_>>();

	let mut vertical_lists = Vec::<Vec<u32>>::new();

	vertical_lists.push(Vec::<u32>::new());
	vertical_lists.push(Vec::<u32>::new());
	vertical_lists.push(Vec::<u32>::new());

	for i in 0..list.len() {
		vertical_lists[i % 3].push(
			list[i].parse::<u32>().unwrap()
		);
	}

	// println!("{:?}", vertical_lists);

	let mut possible_count = 0;
	let mut side_count = 0;
	let mut temp_sides: Vec<u32> = vec![];
	for i in 0..vertical_lists.len() {
		for j in 0..vertical_lists[i].len() {
			side_count += 1;
			temp_sides.push(vertical_lists[i][j]);
			if side_count == 3 {
				side_count = 0;
				if is_possible(&temp_sides) {
					possible_count += 1;
				}
				temp_sides.clear();
			}
		}
	}

	println!("part2: {}", possible_count);

}

