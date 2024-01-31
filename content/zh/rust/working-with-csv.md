---
title:                "处理 CSV 文件"
date:                  2024-01-19
simple_title:         "处理 CSV 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
CSV，即逗号分隔值，是存储表格数据的简单格式。程序员用它因为它通用，容易读写，且与电子表格和数据库兼容。

## How to: (如何操作：)
导入 `csv` 和 `serde` 包来处理CSV文件：

```Rust
use std::error::Error;
use std::fs::File;
use std::process;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Record {
    // 按CSV文件列定义字段
    city: String,
    population: u32,
    latitude: f64,
    longitude: f64,
}

fn run() -> Result<(), Box<dyn Error>> {
    let file_path = "cities.csv";
    let file = File::open(file_path)?;

    let mut rdr = csv::Reader::from_reader(file);
    for result in rdr.deserialize() {
        let record: Record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn main() {
    if let Err(err) = run() {
        println!("error running example: {}", err);
        process::exit(1);
    }
}
```

输出示例（sample output）：
```plaintext
Record { city: "Beijing", population: 21500000, latitude: 39.903, longitude: 116.401 }
Record { city: "Shanghai", population: 24200000, latitude: 31.227, longitude: 121.549 }
...
```

## Deep Dive (深入探索)
CSV起源于20世纪初，用于早期计算机和打印机。JSON、XML、YAML是替代格式，但CSV因其简单性通常优选。Rust里处理CSV要借助库，如`csv`，这库从1.0版就存在。`serde`用于序列化，让结构体与CSV行互转自如。

## See Also (另请参阅)
- 官方`csv`库文档：https://docs.rs/csv/latest/csv/
- `Serde`项目官网：https://serde.rs/
- 更多CSV处理例子和练习：https://github.com/BurntSushi/rust-csv/tree/master/examples
