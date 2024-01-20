---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:14:36.642319-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由?)
プログラマーが現在の日付を取得する行為、それはシステムの時刻情報を把握するためです。日付データはログ、スケジューリング、期限管理などで必要になります。

## How to: (方法)
Gleamでは、現在の日付を取得するには、標準ライブラリに含まれる関数を使用します。以下にコードのサンプルを示します。

```gleam
import gleam/calendar
import gleam/io

pub fn main() {
  case calendar.local_now() {
    Ok(now) -> io.println("現在の日付は: " ++ now.to_string())
    Error(err) -> io.println("エラーが発生しました: " ++ err)
  }
}
```

これを実行すると、こんな感じの出力になります。

```
現在の日付は: 2023-04-01T12:34:56
```

ただし、実際の出力は実行した日時によって異なります。

## Deep Dive (深掘り)
過去、日付と時刻の取得にはさまざまなライブラリがありました。Gleamの`calendar`モジュールはErlangの`calendar`と`datetime`モジュールを参考にしており、扱い易いAPIで現在の日付や時刻を提供します。Gleamでは型安全性を重視しているため、エラー処理もしっかりしています。例えば、`local_now`は成功すれば`Ok`を、問題があれば`Error`を返します。

また、他言語のライブラリと比較すると、Gleamの実装はErlangのVM上で動くため、分散システムやフォールトトレラントなシステムの日付と時刻の処理に優れています。

## See Also (参考情報)
- Gleam documentation for the `calendar` module: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Erlang's `calendar` module: https://erlang.org/doc/man/calendar.html
- More about Erlang's date and time handling: https://erlang.org/doc/apps/erts/time_correction.html

これらのリンクから、さらに日付と時刻の取り扱いについての情報を見つけることができます。