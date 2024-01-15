---
title:                "現在の日付の取得"
html_title:           "Gleam: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

今日の日付を取得することの重要性について、説明します。
プログラミングにおいて、日付は非常に重要な要素であり、現在の日付を取得することはプログラミングのさまざまな用途に役立ちます。例えば、データのタイムスタンプを作成する時や、タスクスケジュールを管理する時などに必要になることがあります。

## 方法

まず、Gleam言語で現在の日付を取得する基本的な方法を見てみましょう。
```Gleam
import gleam/calendar

fn main() {
  let today = calendar.local_today()
  io.println("Today is " ++ show(today))
}
```

上記のコードを実行すると、現在の日付が出力されます。例えば、今日が2021年7月12日であれば、以下のような出力が得られます。

```
Today is 2021-07-12
```

また、より詳細な日付情報を取得することも可能です。例えば、曜日や時刻などを含めて現在の日付を取得するには、以下のようなコードを使用します。

```Gleam
import gleam/calendar, gleam/int/Int

fn main() {
    let today = calendar.local_today()
    let weekday = Int.to_string(calendar.weekday(today))
    let time = Int.to_string(calendar.hours(today)) ++ ":" ++ Int.to_string(calendar.minutes(today))
    io.println("Today is " ++ show(weekday) ++ ", " ++ show(today) ++ " at " ++ show(time))
}
```

上記のコードを実行すると、今日の曜日と時刻を含むより詳細な日付情報が出力されます。

```
Today is 2, 2021-07-12 at 12:30
```

さらに、異なるタイムゾーンで日付を取得することも可能です。例えば、アメリカのニューヨークの現在の日付を取得するには、以下のようなコードを使用します。

```Gleam
import gleam/calendar

fn main() {
    let today = calendar.local_today(calendar.load_location("America/New_York"))
    io.println("Today in New York is " ++ show(today))
}
```

上記のコードを実行すると、現在のニューヨーク時間の日付が出力されます。

```
Today in New York is 2021-07-12
```

## 深堀り

Gleamの読みやすいシンタックスを活用して、現在の日付を取得するためのさまざまな方法があります。例えば、特定の書式で日付を取得することも可能です。

```Gleam
import gleam/calendar

fn main() {
  let today = calendar.local_today()
  let formatted_date = calendar.format("%d/%m/%Y", today)
  io.println("Today's date is " ++ show(formatted_date))
}
```

上記のコードでは、日付を「日/月/年」の書式で取得し、出力された日付は以下のようになります。

```
Today's date is 12/07/2021
```

さらに、日付の算術演算を行うことも可能です。例えば、現在から10日後の日付を取得するには、以下のようにコードを記述します。

```Gleam
import gleam/calendar, gleam/datetime

fn main() {
  let current_date = datetime.now()
  let ten_days_from_now = datetime.add_day(current_date, 10)
  io.println("Ten days from now is " ++ show(ten_days_from_now))
}
```

上記のコードを実行すると、現在から10日後の日付が取