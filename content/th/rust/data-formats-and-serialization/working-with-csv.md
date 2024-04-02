---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:35.656615-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E2D\u0E48\u0E32\u0E19\u0E08\u0E32\u0E01\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E25\u0E07\u0E44\u0E1B\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E18\u0E23\u0E23\u0E21\u0E14\u0E32\u0E17\u0E35\u0E48\u0E40\
  \u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\u0E1A\u0E15\u0E32\
  \u0E23\u0E32\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.017210-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E44\
  \u0E1F\u0E25\u0E4C CSV (Comma-Separated Values) \u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\
  \u0E2D\u0E48\u0E32\u0E19\u0E08\u0E32\u0E01\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E25\u0E07\u0E44\u0E1B\u0E43\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\u0E2D\
  \u0E04\u0E27\u0E32\u0E21\u0E18\u0E23\u0E23\u0E21\u0E14\u0E32\u0E17\u0E35\u0E48\u0E40\
  \u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E41\u0E1A\u0E1A\u0E15\u0E32\
  \u0E23\u0E32\u0E07\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A CSV"
weight: 37
---

## คืออะไร & ทำไม?
การทำงานกับไฟล์ CSV (Comma-Separated Values) คือการอ่านจากและเขียนลงไปในไฟล์ข้อความธรรมดาที่เก็บข้อมูลแบบตาราง โปรแกรมเมอร์ทำเช่นนี้เพื่อเปิดโอกาสให้แชร์ข้อมูลระหว่างโปรแกรมต่างๆ, ระบบต่างๆ หรือสำหรับการจัดการชุดข้อมูลขนาดใหญ่อย่างมีประสิทธิภาพและอ่านง่ายสำหรับมนุษย์

## วิธีการ:
Rust ด้วยความสนใจในเรื่องความปลอดภัยและประสิทธิภาพ ได้เสนอ crates (ไลบรารี) ที่ยอดเยี่ยมสำหรับการจัดการกับไฟล์ CSV โดย `csv` เป็นตัวที่ได้รับความนิยมมากที่สุด คุณก็จำเป็นต้องใช้ `serde` สำหรับการ serialize และ deserialize ข้อมูล

ก่อนอื่นเพิ่ม dependencies ในไฟล์ `Cargo.toml` ของคุณ:

```toml
[dependencies]
csv = "1.1"
serde = { version = "1.0", features = ["derive"] }
```

### การอ่าน CSV

ในการอ่านไฟล์ CSV กำหนด struct ที่แสดงถึงข้อมูลของคุณและ derive `Deserialize` จาก `serde`:

```rust
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io;
use std::process;

#[derive(Debug, Deserialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn read_from_csv(file_path: &str) -> Result<(), Box<dyn Error>> {
    let file = File::open(file_path)?;
    let mut rdr = csv::Reader::from_reader(file);

    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = read_from_csv("cities.csv") {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
```

ตัวอย่างผลลัพธ์สำหรับ CSV ที่มีข้อมูลเมืองอาจดูเช่นนี้:
```plaintext
Record { city: "Seattle", state: "WA", population: 744955 }
Record { city: "New York", state: "NY", population: 8336817 }
```

### การเขียนลงไปใน CSV

ในการเขียนลงไปในไฟล์ CSV กำหนด struct และ derive `Serialize`:

```rust
use serde::Serialize;
use std::error::Error;
use std::fs::File;

#[derive(Serialize)]
struct Record {
    city: String,
    state: String,
    population: u64,
}

fn write_to_csv(file_path: &str, records: Vec<Record>) -> Result<(), Box<dyn Error>> {
    let file = File::create(file_path)?;
    let mut wtr = csv::Writer::from_writer(file);

    for record in records {
        wtr.serialize(&record)?;
    }
    wtr.flush()?;
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let records = vec![
        Record {
            city: "Los Angeles".into(),
            state: "CA".into(),
            population: 3979563,
        },
        Record {
            city: "Chicago".into(),
            state: "IL".into(),
            population: 2695598,
        },
    ];

    write_to_csv("output.csv", records)?;

    Ok(())
}
```

นี่จะสร้าง `output.csv` ด้วยข้อมูล:

```csv
city,state,population
Los Angeles,CA,3979563
Chicago,IL,2695598
```

โดยใช้ประโยชน์จากระบบประเภทข้อมูลที่มีประสิทธิภาพของ Rust และ crates ที่เชื่อถือได้จากระบบนิเวศ การทำงานด้วยข้อมูล CSV จึงทำได้อย่างทั้งมีประสิทธิภาพและง่ายดาย รับประกันความปลอดภัยและประสิทธิภาพในงานการประมวลผลข้อมูลของคุณ
