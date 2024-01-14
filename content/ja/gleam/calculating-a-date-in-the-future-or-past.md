---
title:                "Gleam: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

将来または過去の日付を計算するのに時間を費やす理由はありますか？それは、プログラマーがアプリケーションで日付を管理したい場合や、特定のイベントやタスクの日付を自動的に更新する必要がある場合に役立ちます。

## 方法

日付を計算するには、Gleamの標準ライブラリである`Time.Date`モジュールを使用します。このモジュールには、`add`関数と`subtract`関数があります。これらを使用して、指定された単位（日、月、年など）を追加または減算することで、将来または過去の日付を生成することができます。

例えば、明日の日付を取得するには以下のようにします。

```Gleam
import Time.Date

let tomorrow = Time.Date.add(Time.now(), 1, Time.Date.day)
```

これにより、現在の日付に1日を追加し、明日の日付を取得することができます。また、日付を指定することもできます。例えば、3日前の日付を取得するには以下のようにします。

```Gleam
import Time.Date

let three_days_ago = Time.Date.subtract(Time.now(), 3, Time.Date.day)
```

さらに、月や年などの単位を指定することもできます。詳細な情報は、Gleamの公式ドキュメントを参照してください。

## 深読み

日付を計算する際には、さまざまな要因を考慮する必要があります。たとえば、うるう年や夏時間などの影響を考える必要があります。また、多言語対応のアプリケーションでは、国や地域ごとの日付の表記方法を調整する必要があるかもしれません。Gleamの`Time.Date`モジュールには、これらの要因を考慮するための関数やオプションが備わっています。深く理解し、アプリケーションに適切に実装することが重要です。

## 参考文献

- [Gleam 公式ドキュメント](https://gleam.run/documentation/)
- [GleamのTimeモジュールについて](https://qiita.com/karszawa/items/1523203bcf16e5f13ee6)