---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するとは、文字列を特定の形式の日付に変換する操作です。プログラマがこれを行うのは、データを適切な形式に照合し、操作できるようにするためです。

## チュートリアル：
以下はGleamのコード例とその出力です。

```gleam
import gleam/try
import gleam/bit_builder

pub fn parse_date_string(date_string: String) -> Result(Date, Nil) {
    date_string
    |> bit_builder.lines()
    |> bit_builder.formatted_date("[YYYY]-[MM]-[DD]")
    |> try.unwrap_or_default()
}

let result = parse_date_string("2022-09-14")
```

このコードは文字列 "2022-09-14" を特定の形式（YYYY-MM-DD）に解析します。

## ディープダイブ
つまり、文字列から日付を解析するというアクションは、歴史的に見て、データ形式が一貫性を欠いていた時代から存在しています。これによりプログラマは、異なる形式の日付データをうまく解析し、ユニバーサルな形式に変換できます。

また他の代替手段としては、手動で文字列を解析し、日付部分を抽出することもできますが、これは時間がかかり、エラープローンです。

Gleamの `bit_builder` ビルダーパッケージの `formatted_date` 関数は、この日付解析を非常に簡単にします。これは、指定した形式で提供された日付の文字列を解析し、年、月、日の値を持つDateオブジェクトを返すことができます。

## 参考リンク
日付の解析についてさらに詳しく知るためのソース :
* [Gleam Documentation](https://gleam.run/documentation/)
* [Date parsing in Gleam - Stack Overflow](https://stackoverflow.com/questions/...)