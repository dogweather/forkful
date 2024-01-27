---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
## 何とは？& なぜ？

CSVは「Comma-Separated Values」の略で、カンマで区切られたデータを表します。プログラマーは簡単にデータを交換・保存するため、CSVを使用します。

## How to:
## どうやって：

```gleam
import gleam/csv
import gleam/io

// CSVデータをパースする関数
pub fn parse_csv(data: String) -> Result(list(list(String)), csv.Error) {
  csv.decode(data)
}

// メイン関数で使用
pub fn main() {
  let data = "name,age\nAlice,30\nBob,25"
  let result = parse_csv(data)

  case result {
    Ok(rows) -> io.print(rows)
    Error(err) -> io.print(err)
  }
}
```

サンプル出力:
```
Ok([["name","age"],["Alice","30"],["Bob","25"]])
```

## Deep Dive
## 探検：

CSVは1972年にIBMで初めて使われました。JSONやXMLのような代替フォーマットも存在しますが、CSVはそのシンプルさで多くの場面で活躍しています。GleamでのCSV操作は、`gleam/csv`ライブラリを使い、パースやエンコーディングを行います。エラーハンドリングもしっかりと行うことが大切です。

## See Also
## 関連情報：

- Gleamの公式ドキュメント：https://gleam.run
- `gleam/csv`ライブラリのドキュメント：https://hexdocs.pm/gleam_csv
- CSV規格に関する情報：https://tools.ietf.org/html/rfc4180
