---
title:                "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
html_title:           "Rust: कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Kyun

File CSV ka pryog data ko store aur exchange karne ke liye ek common format hai, jisse ki kai applications CSV files ke sath kaam karte hain. Isliye, CSV files ke maamle mein bhi coding abilities hona kaafi faydemand hota hai.

## Kaise Karein

Sabse pehle, hume "csv" library ko apne Rust project mein add karna hoga. Iske baad hum CSV file ko read aur write kar sakte hain. Niche diye gaye code blocks mein aap dekh sakte hain ki kaise hum file ko read aur write kar sakte hain.

```Rust
use csv; //csv library import 
fn main() {
  let mut reader = csv::Reader::from_path("data.csv").unwrap(); //read from csv file 
  for result in reader.records() {
    let record = result.unwrap();
    println!("{:?}", record); //printing the record 
  }
}
```
```
//sample output
["John", "Doe", "john.doe@email.com", "12345"]
["Jane", "Smith", "jane.smith@email.com", "67890"]
["Bob", "Johnson", "bob.johnson@email.com", "13579"]
```
```Rust
use csv; //csv library import 
fn main() {
  let mut writer = csv::Writer::from_path("new_data.csv").unwrap(); //write to csv file 
  writer.write_record(&["Name", "Email", "Phone"]); //write headers 
  writer.write_record(&["Peter", "peter@email.com", "09876"]); //write data 
  writer.write_record(&["Emma", "emma@email.com", "54321"]); 
  writer.flush(); //flush writer to save changes 
}
```
```
//sample output in new_data.csv file 
Name,Email,Phone
Peter,peter@email.com,09876
Emma,emma@email.com,54321
```

## Deep Dive

CSV files mein data comma separated format mein hota hai, jiske liye hum "," ko delimeter ke roop mein use karte hain. Agar hume specifically kisi column ke data ko access karna ho, toh hum "headers" aur "index" ka pryog kar sakte hain. Iske alawa, hum "serde_csv" library ka bhi pryog kar sakte hain jo ki CSV files ko parse karne ke liye kaafi flexible aur efficient hai.

## Dekhiye Bhi

- [CSV Crate Documentation](https://docs.rs/csv/)
- [Serde CSV Crate Documentation](https://docs.serde.rs/serde_csv/)
- [Rust Language Documentation](https://www.rust-lang.org/hi/learn)