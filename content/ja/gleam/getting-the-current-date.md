---
title:                "Gleam: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

＃＃　なぜ

日付を取得する理由は多々あります。例えば、データベースやファイルの作成日時を取得するために使用したり、特定の作業を実行するために必要な正確な時刻を取得したりするためにも使用することができます。

＃＃＃　Ｈｏｗ　Ｔｏ

日付を取得するには、まず「gleam/datetime」ライブラリをインポートします。次に、```Gleam.datetime.now()```を使用して現在の日付を取得します。また、日付をフォーマットする必要がある場合は、```Gleam.datetime.format()```を使用することができます。

例：
```Gleam
import gleam/datetime

let current_date = Gleam.datetime.now()

let formatted_date = Gleam.datetime.format("YYYY-MM-DD hh:mm:ss", current_date)

## Deep Dive

日付を取得する方法についてもっと詳しく知りたい場合、以下の点に注意してください：

- ライブラリをインポートする方法
- 「now()」を使用して日付を取得する際のオプションフィールド
- フォーマットすることで取得した日付を表示する方法

## 関連リンク

[マニュアル：gleam/datetime](https://gleam.run/manuals/libraries/datetime.html)
[サンプルコード：日付を取得する方法](https://github.com/gleam-lang/gleam/blob/master/examples/datetime/datetime.gleam)