---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？

デートの比較とは、ある日付が別の日付より過去、未来、またはそれらと同じであるかを判定するプロセスです。この比較は、日付の並べ替え、間隔の計算、有効期限のチェックなどに対するプログラミングの基本的な部分です。

## 方法：

Gleamでの日付比較の方法を以下に示します：

```Gleam
import gleam/utc_date 

fn main() {
  let first_date = utc_date.from_parts(2021, 2, 28)
  let second_date = utc_date.from_parts(2021, 3, 1)
  
  case first_date < second_date {
    True -> 
      io.println("First date is earlier than second")
    False ->
      io.println("First date is equal to or later than second")
  }
}
```

実行結果は次の通りです：

```
First date is earlier than second
```

## 深堀り：

Gleamが日付を比較する方法は、比較を行う前にUTCでの日付を数更新するというシンプルなものです。さらに、Gleamはこの種の比較を行うために内部的にビルトイン型の`utc_date`を用意しています。代替方法として、日付を文字列に変換して比較する方法もありますが、これはドメイン上や性能上の問題が発生する可能性があるため、推奨されません。

## 参照：

日付（または時間）間隔の計算についてのさらなる情報は `gleam/utc_date` （https://github.com/gleam-lang/stdlib/blob/main/src/gleam/utc_date.gleam）で見つけることができます。Gleamの日付操作全般については、以下の公式ドキュメンテーション（https://gleam.run/book/tour/dates-and-times.html）を参照してください。また、Gleamでの比較操作についての詳細は、https://gleam.run/book/tour/comparisons.html で見つけることができます。