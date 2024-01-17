---
title:                "現在の日付を取得する"
html_title:           "Gleam: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何が？なぜ？
現在の日付を取得するとは何かを説明するために、プログラマがこれを行う理由について説明します。
大部分のプログラムには、現在の日付を取得する必要があるからです。これにより、特定の日時を追跡したり、時間に基づいて処理を変更したりすることができます。

## 方法：
```Gleam
let current_date = Time.now()
```
上記のコードを使用して、現在の日付をGleamで取得することができます。コードを実行すると、出力は以下のようになります。

```Gleam
2021-01-07T15:42:22.658712Z
```
上記のように、日付や時間を表示するフォーマットを指定することもできます。例えば、 `Time.format (current_date, "%Y/%m/%d")` とすると、以下のように表示されます。
```Gleam
2021/01/07
```

## 詳しく見る：
### 歴史的背景
日付や時間を取得するための標準的な手段はたくさんありますが、その中でも最も一般的な方法は、『エポック時間』と呼ばれる1970年1月1日からの経過秒数を使用する方法です。しかし、この方法はシステム毎に異なるエポック時間を持っている場合があり、扱いにくくなることがあります。

### 代替手段
他にも、Dateモジュールを使用して日付や時間を取得する方法もあります。このモジュールを使用すると、日付や時間をフォーマットするための多様なオプションが提供されます。

### 実装の詳細
Gleamでは、 `Time` モジュールを使用して日付や時間を取得することができます。バックエンドで `Time` は、エポック時間を使用して日付や時間を計算します。また、 `Time` モジュールには、日付や時間を操作するための多数の関数が用意されています。

## 関連情報を参照：
- [GleamのTimeモジュールのドキュメント](https://gleam.run/libraries/time)
- [Dateモジュールを使用して日付や時間を取得する方法の例](https://github.com/gleam-lang/gleam/blob/master/examples/date_time/date.gleam)