---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Swiftでの日付の文字列変換について (Convert a Date To a String in Swift)

## 何となぜ？
日付を文字列に変換するとは、具体的には日時データをString型に変換することを言います。これが必要なのは、ユーザーが見やすい形式で日付を表示したり、日付データを文字列として保存、または送信する場合などです。

## 手順:
Swiftで簡単に日付を文字列に変換するコードは以下の通りです:

```Swift
import UIKit

let now = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy年MM月dd日 HH時mm分ss秒"
let stringDate = formatter.string(from: now)
print(stringDate)
```

コードを実行すると、例えばこんな出力が得られます:

```Swift
2022年08月06日 12時45分00秒
```

ここでは `DateFormatter` を使って特定の日付形式の文字列を作成します。

## ディープダイブ
元々、Swiftでは `NSDateFormatter` というクラスが提供されていたが、Swift 2から `DateFormatter` に名前が変更され、使い方も簡易化された。また、日付形式を自分で指定する方法以外にも、既定のスタイル（.short、.medium、.long、.full）を利用することも可能だ。

他言語を比較した時、SwiftはISO8601やRFC3339といった一般的な日付形式に対応している点が特徴的だ。更に詳しく言うと、Swiftの `Date` は1970年1月1日からの秒数を内部的に保持していて、それを元に日付に対する各種計算を行っている。

## 関連リンク
- Swift公式サイト: https://www.swift.org
- DateFormatterクラスの詳細（Apple公式ドキュメント）: https://developer.apple.com/documentation/foundation/dateformatter
- Swift日付の操作について（Qiita記事）: https://qiita.com/yucovin/items/95fa044f1ce6bb8536f1

以上、Swiftでの日付の文字列変換についてでした。