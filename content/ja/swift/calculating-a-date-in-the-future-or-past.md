---
title:                "未来または過去の日付を計算する"
html_title:           "Swift: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# デート計算：Swiftで行う方法

## 何となぜ？: なぜ開発者が日付計算をするのか？
日付の計算は、私たちが将来または過去の特定の日付を取得するための方法です。これは、リマインダーや予定管理、時間枠の設定など、多くのアプリケーションで必須の機能となっています。

## 方法:
以下は、Swiftで未来の日付を計算するサンプルコードです。

```Swift
import Foundation

let today = Date()
let futureDate = Calendar.current.date(byAdding: .day, value: 10, to: today)

print(futureDate)
```
このコードを実行すると、今日から10日後の日付が出力されます。

過去の日付を計算する場合、以下のようになります。

```Swift
import Foundation

let today = Date()
let pastDate = Calendar.current.date(byAdding: .day, value: -10, to: today)

print(pastDate)
```
このコードを実行すると、今日から10日前の日付が出力されます。

## ディープダイブ
日付計算は、具体的な日付を取得するための基本的な方法で、多くのプログラミング言語で実装されています。Swiftでは、Foundationフレームワークの一部である`Calendar`と`Date`を使います。

代替方法としては、自分自身で日付計算の関数を作成することもできますが、独自のルールを適用する必要がない限り、標準ライブラリを使用することが推奨されます。さらに、時間帯やカレンダーに特有の問題（例：夏時間、閏年）に対応するため、この計算は時折複雑になります。

## 参考文献
1. [Apple Developer Documentation: Date](https://developer.apple.com/documentation/foundation/date)
2. [Apple Developer Documentation: Calendar](https://developer.apple.com/documentation/foundation/calendar)

Swiftの日付計算は練習を通じて自然に身につけられます。公式ドキュメンテーションで更に詳しい情報を探してみてください。