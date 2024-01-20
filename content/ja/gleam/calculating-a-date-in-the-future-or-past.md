---
title:                "未来または過去の日付を計算する"
html_title:           "Gleam: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
未来または過去の日付の計算とは、現在の日付から特定の時間単位（日、週、月、年など）を加算または減算して、特定の未来または過去の日付を導く手法です。プログラマーはこれを行うことで、日付ごとのデータ追跡、予定管理、記念日のリマインダーなど、多くのアプリケーションが必要とする機能を実装します。

## 手順：
Gleam言語を使って未来の日付を計算する例を見てみましょう。
```Gleam
import gleam/time.{as_datetime, future}
let future_date = 
  as_datetime("2023-02-20T15:30:00Z") 
  |> future(years: 1) 
```
このコードは、2023年2月20日から1年後の日付を計算します。

過去の日付を計算する例も見てみましょう。
```Gleam
import gleam/time.{as_datetime, past}
let past_date = 
  as_datetime("2023-02-20T15:30:00Z") 
  |> past(days: 365)
```
このコードは、2023年2月20日から1年前の日付を計算します。

## ディープダイブ：
- 歴史的な文脈: 
    日付演算は古代からの実践であり、今日でもこれらの計算は必要です。この領域では特別な問題もあります：リープ年、時差、夏時間など、地域ごとに異なる仕様が取り入れられています。

- 代替案： 
    Gleam言語以外では多くのプログラミング言語、（例えばJavaScript、Python、Rubyなど）が日付の計算をサポートしています。

- 実装の詳細: 
    Gleamの`future`と`past`関数は、Unixエポックからのミリ秒として表される日時を操作します。これらの関数は、指定された時間単位をミリ秒に変換し、元の日時に追加または減算します。

## 参照：
- Gleam公式ドキュメンテーション: [https://gleam.run/docs/](https://gleam.run/docs/)
- オンライン書籍 "Programming Gleam": [https://www.programming-elixir.com/](https://www.programming-elixir.com/)
- Gleamコミュニティフォーラム: [https://gleam.run/forum/](https://gleam.run/forum/)