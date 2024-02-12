---
title:                "CSVファイルの操作"
aliases:
- /ja/rust/working-with-csv/
date:                  2024-01-19
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV（Comma-Separated Values）はデータを保存するのによく使われるシンプルなフォーマットです。開発者はCSVを用いて、データのインポート、エクスポート、解析が簡単で、さまざまなプログラム間で互換性を持たせるために使います。

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
