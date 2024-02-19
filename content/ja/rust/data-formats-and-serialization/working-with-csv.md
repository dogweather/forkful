---
aliases:
- /ja/rust/working-with-csv/
date: 2024-01-19
description: "CSV\uFF08Comma-Separated Values\uFF09\u306F\u30C7\u30FC\u30BF\u3092\u4FDD\
  \u5B58\u3059\u308B\u306E\u306B\u3088\u304F\u4F7F\u308F\u308C\u308B\u30B7\u30F3\u30D7\
  \u30EB\u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u958B\u767A\u8005\
  \u306FCSV\u3092\u7528\u3044\u3066\u3001\u30C7\u30FC\u30BF\u306E\u30A4\u30F3\u30DD\
  \u30FC\u30C8\u3001\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u89E3\u6790\u304C\u7C21\
  \u5358\u3067\u3001\u3055\u307E\u3056\u307E\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u9593\
  \u3067\u4E92\u63DB\u6027\u3092\u6301\u305F\u305B\u308B\u305F\u3081\u306B\u4F7F\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.741214
summary: "CSV\uFF08Comma-Separated Values\uFF09\u306F\u30C7\u30FC\u30BF\u3092\u4FDD\
  \u5B58\u3059\u308B\u306E\u306B\u3088\u304F\u4F7F\u308F\u308C\u308B\u30B7\u30F3\u30D7\
  \u30EB\u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u958B\u767A\u8005\
  \u306FCSV\u3092\u7528\u3044\u3066\u3001\u30C7\u30FC\u30BF\u306E\u30A4\u30F3\u30DD\
  \u30FC\u30C8\u3001\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u89E3\u6790\u304C\u7C21\
  \u5358\u3067\u3001\u3055\u307E\u3056\u307E\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u9593\
  \u3067\u4E92\u63DB\u6027\u3092\u6301\u305F\u305B\u308B\u305F\u3081\u306B\u4F7F\u3044\
  \u307E\u3059\u3002"
title: "CSV\u30D5\u30A1\u30A4\u30EB\u306E\u64CD\u4F5C"
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
