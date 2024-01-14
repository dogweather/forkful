---
title:                "Gleam: 現在の日付を取得する"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
日付を取得するのに興味がある人は、Gleamでの日付の取得方法を学ぶことで、より使いやすくエレガントなコードを書くことができるようになります。また、日付はプログラミングで非常によく使用される基本的な要素であるため、その機能を理解することは非常に重要です。

## 使い方
```Gleam
import gleam/time

let current_date = time.now()
|> time.format_date(time.format.rfc_3339)
|> IO.print
```
上記のコードを実行すると、現在の日付と時刻がRFC 3339のフォーマットで出力されます。他のフォーマットを使用する場合は、`time.format`モジュールのドキュメントを参照してください。

## ディープダイブ
日付を取得するとき、われわれのコンピュータは実際にはUTC時間を使用しています。`time.now()`は、システムのタイムゾーン設定に基づいて日付を調整します。また、`time.now()`は関数ですが、第二引数としてタイムゾーン情報を受け取ることもできます。より詳細な情報については、`gleam/time`モジュールのドキュメントを参照してください。

## 詳しくはこちらを参照
- Gleam公式ドキュメント: https://gleam.run/libraries/time/
- RFC 3339フォーマットの説明: https://www.ietf.org/rfc/rfc3339.txt