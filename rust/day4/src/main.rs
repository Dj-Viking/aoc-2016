use std::fs;
use std::collections::HashSet;

fn main() {
	part1();
}

fn part1() {
	let file = fs::read_to_string("./sample").unwrap();

	let roomchecksum_groups = file
		.split("\n")
		.filter(|x| !x.is_empty())
		.map(|x| x.split("[")
				 .map(|x| x.to_string().replace("]", ""))
				 .collect::<Vec<_>>()
		)
	.collect::<Vec<_>>();

	let mut id_sum = 0;

	roomchecksum_groups
		.into_iter()
		.for_each(|mut grp| {
			println!("=========");
			println!("{:?}", grp);

			let mut id = 0;
			let mut checksum = String::new();
			let mut room = grp[0]
				.split("-")
				.map(|x| x.to_string())
				.collect::<Vec<_>>();

			grp[1]   = grp[1].to_string();
			checksum = grp[1].clone();

			println!("room and id {:?}", room);

			id = room.pop().unwrap().parse::<u32>().unwrap();

			if is_real_room(&room, checksum) {
				id_sum += id;
			}
		});
	// 184955 too high!!!
	println!("part1: {:?}", id_sum);
}

// todo: what to do here...
//A room is real (not a decoy) 
//if the checksum is the five most common letters 
//in the encrypted name, 
//in order, 
//with ties broken by alphabetization. 
fn is_real_room(room: &Vec<String>, checksum: String) -> bool{
	let roomstr = room.join("");
	let mut result = false;

	println!("room {:?}", room);
	result
}
