---
date: 2024-01-19
description: "How to: Rust\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u57FA\
  \u672C\u7684\u306A\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.853442-06:00'
model: unknown
summary: "Rust\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u57FA\u672C\u7684\
  \u306A\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059."
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
