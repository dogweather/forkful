---
title:                "「未来または過去の日付を計算する」"
html_title:           "Gleam: 「未来または過去の日付を計算する」"
simple_title:         "「未来または過去の日付を計算する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜこれをするのか
日付を未来や過去に計算する理由は様々ですが、特に永続性のあるアプリケーションでは、将来の予定や過去の出来事を正確に表示する必要があります。Gleamの日付計算の機能は、このようなアプリケーションを開発するために非常に便利です。

## 方法
以下のコードブロックには、Gleamを使用して未来の日付を計算する方法の例が示されています。
```Gleam
import gleam/time.{Month, Year}

// 今日の日付
let today = Time.now()

// 100日後の日付を計算
let future = Time.advance(today, days=100)

// 結果を出力
IO.print("100日後の日付は ", future)

// もっと複雑な例
let future_date = Time.advance(today, years=1, months=3, days=12)

// 結果を出力
IO.print("1年3ヶ月12日後の日付は ", future_date)
```

上記のコードを実行すると、現在の日付から100日後の日付が表示されます。
また、より複雑な例では、現在の日付から1年3ヶ月12日後の日付が計算されます。

## 深堀り
日付の計算には様々なパラメーターを指定することができます。
例えば、年や月だけでなく週や時間、分なども指定することができます。
さらに、特定の時刻を基準にして計算することも可能です。
詳細な使い方は、公式ドキュメントを参照してください。

## 以下は参考になるリンクです。

[公式ドキュメント](https://gleam.run/manual/standard-library.html#time)
[GleamのGitHubリポジトリ](https://github.com/gleam-lang/gleam/blob/master/stdlib/time/README.md)