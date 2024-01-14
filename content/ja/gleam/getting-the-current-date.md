---
title:    "Gleam: 現在の日付の取得方法"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# なぜ

最新の日付を取得するのに関わる理由は、特定の日付が必要な場合やプログラムで日付を使用することが必要な場合があるためです。

## 方法

```Gleam
import Time.Date

// 現在の日付を取得
let current_date = Time.Date.now()

// 取得した日付を表示
IO.print("今日の日付は ${Date.format(current_date, "%m/%d/%Y")} です。\n")
```

このように、GleamではTime.Dateモジュールを使用して現在の日付を取得し、指定したフォーマットで表示することができます。

```Gleam
// 日付を取得してから2日後の日付を計算
let today = Time.Date.now()
let two_days_later = Time.Date.add_days(today, 2)

// 計算した日付を表示
IO.print("2日後の日付は ${Date.format(two_days_later, "%m/%d/%Y")} です。\n")
```

上記のように、Time.Dateモジュールを使用することで日付に対する様々な操作が可能です。

## 深層

Gleamでは、Time.Dateモジュール以外にも日付に関わる操作が行えるモジュールがあります。例えば、Time.DateTimeモジュールを使用することで、時間の情報も含めた日付の取得や操作が可能です。

また、Time.Dateモジュールのメソッドを使用する際には、引数に渡すフォーマットのパターンについてもより詳しく学ぶことができます。

# これらも参考にしてください

- [Gleam公式ドキュメント: Timeモジュール](https://gleam.run › std › gleam.time)
- [Timeモジュールのフォーマットパターンについて](https://gleam.run › std › gleam.time › date › formatting)