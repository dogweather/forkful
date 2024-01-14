---
title:                "Rust recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSV (Comma-Separated Values) is a common file format for storing tabular data. If you are working with data analysis or manipulating large datasets, understanding how to work with CSV files can be incredibly useful. In this tutorial, we will explore how to work with CSV files in Rust. Whether you are a beginner or an experienced programmer, Rust's strong type system and memory safety will make CSV manipulation a breeze.

## How To

Before diving into code, let's first take a look at a sample CSV file that we will be working with. Suppose we have a CSV file called `cars.csv` containing information about different car models and their specifications.

```
Make, Model, Year, Price
Toyota, Camry, 2018, $24,000
Honda, Civic, 2017, $19,500
Ford, Mustang, 2016, $30,000
```

To read this file in Rust, we first need to add the `csv` crate to our `Cargo.toml` file:

```
[dependencies]
csv = "1.1.1"
```

Now, let's open and read the file using the `csv` crate:

```rust
use csv::ReaderBuilder;

fn main() {
    let mut reader = ReaderBuilder::new()
        .has_headers(true)
        .from_path("cars.csv")
        .unwrap();

    for result in reader.records() {
        let record = result.unwrap();
        let make: String = record[0].to_string();
        let model: String = record[1].to_string();
        let year: u32 = record[2].parse().unwrap();
        let price: f64 = record[3].parse().unwrap();

        println!("The {} {} {} costs ${}", year, make, model, price);
    }
}
```

Running this code will give us the following output:

```
The 2018 Toyota Camry costs $24000.0
The 2017 Honda Civic costs $19500.0
The 2016 Ford Mustang costs $30000.0
```

Here, we used a `Reader` from the `csv` crate to read the CSV file and extract data from each row. Notice how we had to specify the data type for each value. In Rust, CSV data is represented as a `String` by default, so we need to explicitly convert it to the appropriate type.

## Deep Dive

Now that we have seen how to read a CSV file, let's dive deeper and explore some other common operations that we may need to perform.

### Writing to a CSV file

We can also use the `Writer` from the `csv` crate to create new CSV files or append data to existing ones. Here's an example of how we can write data to a new CSV file:

```rust
use csv::Writer;

fn main() {
    let mut writer = Writer::from_path("new_cars.csv").unwrap();

    writer.write_record(&["Make", "Model", "Year", "Price"]).unwrap();
    writer.write_record(&["Chevrolet", "Corvette", "2021", "$60,000"]).unwrap();
    writer.write_record(&["BMW", "X5", "2020", "$70,000"]).unwrap();

}
```

Running this code will create a new file called `new_cars.csv` with the following content:

```
Make,Model,Year,Price
Chevrolet,Corvette,2021,$60,000
BMW,X5,2020,$70,000
```

### Working with headers

In the previous examples, we have been assuming that our CSV file has headers (i.e. the first row contains the names of each column). However, if our file doesn't have headers, we can use the `ReaderBuilder` to specify that and access the data differently:

```rust
use csv::ReaderBuilder;

fn main() {
    let mut reader = ReaderBuilder::new()
        .has_headers(false)
        .from_path("cars.csv")
        .unwrap();

    for result in reader.records() {
        let record = result.unwrap();
        let make: String = record[0].to_string();
        let model: String = record[1].to_string();
        let year: u32 = record[2].parse().unwrap();
        let price: f64 = record[3].parse().unwrap();

        println!("The {} {} {} costs ${}", year, make, model, price);
    }
}
```

In this case, we access the data by index instead of using the header names.

### Dealing with errors

When working with real-world data, it's important to handle errors gracefully. In CSV manipulation, we may encounter errors such as missing data or incompatible data types. Luckily, the `csv` crate provides us with methods