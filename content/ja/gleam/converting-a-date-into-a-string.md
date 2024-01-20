---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換するとは、日付データを文字列形式に換算することを指します。これは、データの保存や表示上の都合で、日付を特定の形式の文字列として扱う必要があるために行われます。

## やり方：

以下に日付を文字列に変換するGleamプログラミングの例を示します。

```Gleam
import gleam/datetime.{day, month, year, format}

fn main() {
  let date = day(28) + month(12) + year(2021)
  let date_string = format(date, "{YYYY}/{MM}/{DD}")
  date_string
}
```

出力：
```Gleam
"2021/12/28"
```

## 深堀り：

日付を文字列に変換する技術は、他の多くのプログラミング言語でも使われています。例えば、Pythonでは`strftime`関数、Javaでは`SimpleDateFormat`クラスがそれに当たります。

Gleamでは、

- `{YYYY}` 年を4桁で表します
- `{MM}` 月を2桁で表します
- `{DD}` 日を2桁で表します

このようなフォーマット指定子で日付を表示する形式をカスタマイズすることができます。

## 関連情報

- Gleam公式ドキュメント： [gleam/datetime](https://hexdocs.pm/gleam_stdlib/gleam/datetime/)
- Gleamコミュニティ： [Gleam Forum](https://community.gleam-lang.org/)
- Gleamチュートリアル: [Learning Gleam](https://gleam.run/getting-started/mix-and-elixir/)

これらのリンクは、日付の文字列変換のほかにも、Gleamの基本的な使い方やその他の技術についての詳細な情報を提供しています。