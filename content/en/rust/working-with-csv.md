---
title:                "Working with csv"
html_title:           "Rust recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma-Separated Values) is a widely used file format for storing and exchanging tabular data. As a Rust programmer, learning how to work with CSV can greatly expand your skills and allow you to handle a wide variety of data formats.

## How To

To start working with CSV in Rust, we first need to add the `csv` crate to our project's dependencies. This can be done by adding the following line to our `Cargo.toml` file:

```
csv = "1.1.1"
```

Next, let's import the `csv` crate in our code by adding the line `use csv;` to the top of our file. Now, we can start reading and writing CSV files using the functionalities provided by the `csv` crate.

### Reading CSV

Let's say we have a CSV file named `data.csv` with the following content:

```
Name,Age,Occupation
John,25,Software Engineer
Jane,28,Data Analyst
Mike,31,Project Manager
```

To read this file in Rust, we can use the `Reader` struct from the `csv` crate. Here's an example of how we can read the `data.csv` file and print out its contents:

```
use csv::Reader;

let mut reader = Reader::from_path("data.csv").expect("Failed to open CSV file.");
for result in reader.records() {
    let record = result.expect("Failed to read record.");
    println!("{:?}", record);
}
```

When we run this code, we should see the following output:

```
["John", "25", "Software Engineer"]
["Jane", "28", "Data Analyst"]
["Mike", "31", "Project Manager"]
```

### Writing CSV

In addition to reading CSV files, the `csv` crate also allows us to write CSV files. Let's say we have a `Vec` of `Person` structs, each representing a person's name, age, and occupation. Here's an example of how we can write this data to a CSV file named `people.csv`:

```
use csv::Writer;

struct Person {
    name: String,
    age: u8,
    occupation: String,
}

let people = vec![
    Person {
        name: "John".to_string(),
        age: 25,
        occupation: "Software Engineer".to_string(),
    },
    Person {
        name: "Jane".to_string(),
        age: 28,
        occupation: "Data Analyst".to_string(),
    },
    Person {
        name: "Mike".to_string(),
        age: 31,
        occupation: "Project Manager".to_string(),
    },
];

let file = File::create("people.csv").expect("Failed to create CSV file.");
let mut writer = Writer::from_writer(file);

for person in &people {
    writer.write_record(vec![
        person.name.as_str(),
        person.age.to_string().as_str(),
        person.occupation.as_str(),
    ]).expect("Failed to write to CSV file.");
}

writer.flush().expect("Failed to flush writer.");
```

After running this code, we should have a `people.csv` file with the following contents:

```
John,25,Software Engineer
Jane,28,Data Analyst
Mike,31,Project Manager
```

## Deep Dive

The `csv` crate offers many more functionalities for handling CSV files, such as custom delimiters, quoting characters, and encoding. It also provides options for handling errors and headers in CSV files. For more information on how to use the `csv` crate, be sure to check out its documentation.

## See Also

Here are some useful links to learn more about working with CSV in Rust:

- [csv documentation](https://docs.rs/csv/1.1.1/csv/)
- [Rust Programming Language website](https://www.rust-lang.org/)
- [Rust community forum](https://users.rust-lang.org/)