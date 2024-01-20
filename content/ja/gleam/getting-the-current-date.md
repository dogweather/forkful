---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
現在の日付を取得することは、その日の日付に基づいた情報の記録または調査など、プログラム内でリアルタイムの日付を使いたいさまざまな状況に対応する手段です。これはログのタイムスタンプを生成したり、特定のタスクを一日に一回だけ実行したりするために頻繁に行われます。

## やり方
以下のGleamコード例は現在の日付を示します:
```Gleam
import gleam/datetime.{Datetime, Now}
fn main() {
  let current_datetime: Result(Datetime, Nil) = Now.now()
  case current_datetime {
    Ok(datetime) -> 
      datetime.year()
      |> Int.to_string
      |> IO.println
    Error(_) -> IO.println("Failed to get current date.")
  }
}
```
このコードを実行すると、現在の年が出力されます。

## ディープダイブ
歴史的に、現在の日付を取得する方法はさまざまなプログラミング言語やシステムで異なります。例えばUnixシステムでは、`time()`関数を使用して1970年1月1日からの秒数を取得し、それを日付に変換します。しかし、Gleamの現在のバージョンでは、組み込みのdatetimeモジュールの`Now.now()`関数を使用します。他の方法としては、外部サービスに問い合わせたり、ユーザーに入力させたりすることもあります。最終的な選択は、用途と精度の要件によります。

## 参照
3. Present time in Unix: [unix.com](https://www.unix.com/man-page/posix/time-2/)

以上が、`Gleam`で現在の日付を取得する基本的な手順です。コードを試したり、参考リンクからさらに学んでみてください。