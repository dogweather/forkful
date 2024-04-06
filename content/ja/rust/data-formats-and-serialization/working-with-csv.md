---
date: 2024-01-19
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.803481-06:00'
model: unknown
summary: "CSV\u306F1972\u5E74\u306BIBM\u3067\u4F7F\u308F\u308C\u59CB\u3081\u307E\u3057\
  \u305F\u3002JSON\u3084XML\u306A\u3069CSV\u306E\u4EE3\u66FF\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u304C\u3042\u308A\u307E\u3059\u304C\u3001CSV\u306F\u305D\u306E\u5358\
  \u7D14\u3055\u304B\u3089\u4F9D\u7136\u3068\u3057\u3066\u5E83\u304F\u4F7F\u308F\u308C\
  \u3066\u3044\u307E\u3059\u3002Rust\u3067CSV\u3092\u6271\u3046\u969B\u3001`csv`\u30AF\
  \u30EC\u30FC\u30C8\u304C\u5E83\u304F\u5229\u7528\u3055\u308C\u3066\u3044\u307E\u3059\
  \u3002\u9AD8\u901F\u3067\u30E1\u30E2\u30EA\u5B89\u5168\u306ACSV\u306E\u8AAD\u307F\
  \u66F8\u304D\u304C\u53EF\u80FD\u3067\u3001Serde\u30E9\u30A4\u30D6\u30E9\u30EA\u3068\
  \u306E\u7D71\u5408\u3082\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u3059\u3002"
title: "CSV\u30D5\u30A1\u30A4\u30EB\u306E\u64CD\u4F5C"
weight: 37
---

## How to:
RustでCSVファイルを扱う基本的な方法を示します。

```Rust
use std::error::Error;
use std::fs::File;
use std::io::{Read, Write};
use csv::ReaderBuilder;
use csv::Writer;

fn main() -> Result<(), Box<dyn Error>> {
    // CSV読み込み
    let mut rdr = ReaderBuilder::new()
        .from_path("data.csv")?;
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }

    // CSV書き込み
    let mut wtr = Writer::from_writer(vec![]);
    wtr.write_record(&["year", "make", "model"])?;
    wtr.write_record(&["2023", "Toyota", "Tundra"])?;
    wtr.flush()?;
    let data = String::from_utf8(wtr.into_inner()?)?;
    println!("{}", data);

    Ok(())
}
```
実行結果：
```
StringRecord(["year", "make", "model"])
StringRecord(["2023", "Toyota", "Tundra"])
year,make,model
2023,Toyota,Tundra
```

## Deep Dive
CSVは1972年にIBMで使われ始めました。JSONやXMLなどCSVの代替フォーマットがありますが、CSVはその単純さから依然として広く使われています。RustでCSVを扱う際、`csv`クレートが広く利用されています。高速でメモリ安全なCSVの読み書きが可能で、Serdeライブラリとの統合もサポートしています。

## See Also
- Rust `csv`クレートドキュメント: https://docs.rs/csv/latest/csv/
- CSVフォーマット: https://tools.ietf.org/html/rfc4180
- Serde: https://serde.rs/
