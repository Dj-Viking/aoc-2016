use std::fs;
use std::collections::HashMap;

fn main() {
	// part1();
	part2();
}

fn part2() {
	let file = fs::read_to_string("./sample").unwrap();

}

fn part1() {
	let file = fs::read_to_string("./input").unwrap();

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
			// println!("=========");
			// println!("{:?}", grp);

			let mut id = 0;
			let mut checksum = String::new();
			let mut room = grp[0]
				.split("-")
				.map(|x| x.to_string())
				.collect::<Vec<_>>();

			grp[1]   = grp[1].to_string();
			checksum = grp[1].clone();

			// println!("room and id {:?}", room);

			id = room.pop().unwrap().parse::<u32>().unwrap();

			if is_real_room(&room, checksum) {
				id_sum += id;
			}
		});
	println!("part1: {:?}", id_sum);
}

//A room is real (not a decoy) 
//if the checksum is the five most common letters 
//in the encrypted name, 
//in order, 
//with ties broken by alphabetization. 
fn is_real_room(room: &Vec<String>, checksum: String) -> bool{
	let roomstr = room.join("");
	let mut hm = HashMap::<char, u32>::new();

	for c in roomstr.chars() {
		if c < 'a' || 'z' < c { continue; }
		*hm.entry(c).or_insert(0) += 1;
	}

	let mut common: Vec<(char, u32)> = hm.clone().into_iter().collect();
	common.sort_by(|a,b| {
		if      hm[&a.0] > hm[&b.0] { std::cmp::Ordering::Less    }
		else if hm[&a.0] < hm[&b.0] { std::cmp::Ordering::Greater }
		else if a.0 < b.0           { std::cmp::Ordering::Less    }
		else if a.0 > b.0           { std::cmp::Ordering::Greater }
		else                        { std::cmp::Ordering::Equal   }
	});

	let mut sorted_common = common.clone().into_iter().map(|kv| {
		kv.0
	})
	.collect::<Vec<_>>();

	// println!("common {:?}", common);

	let check = sorted_common
		.splice(0..5, vec![]).map(|x|x.to_string()).collect::<Vec<_>>().join("");

	// println!("sorted common cut {:?}", check);

	// println!("room {:?}", room);
	check == checksum
}
