---
title:                "日付の比較"
html_title:           "Swift: 日付の比較"
simple_title:         "日付の比較"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何？どうして？

日付を比較するとは、プログラマーが2つの日付を比べて、どちらがより前か後かを判断することです。プログラマーが日付を比較する理由は、例えば、予約システムや期限の管理など、日付に関する処理を行うためです。

## 方法:

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"

// 日付を作成
let date1 = formatter.date(from: "2021-01-08")
let date2 = formatter.date(from: "2021-02-14")

if let date1 = date1, let date2 = date2 {
    // 日付を比較
    if date1 < date2 {
        print("\(date1)は\(date2)より前です。")
    } else {
        print("\(date1)は\(date2)より後です。")
    }
}
```

**出力:**

```
2021-01-08は2021-02-14より前です。
```

## 詳細を深く:

- この日付の比較は、コンピューターが日付を数字として扱うようになった1949年以降、プログラミングの基本的な処理の一つとして考えられています。
- 上記の例では、`<`演算子を使って日付を比較しましたが、`compare()`メソッドを使うこともできます。
- さらに、`Calendar`クラスを使って、曜日や時間帯など、より詳細な比較も可能です。

## 関連リンク:

- [Swiftプログラミング言語](https://www.apple.com/jp/swift/)
- [Foundation公式ドキュメント](https://developer.apple.com/documentation/foundation/date)