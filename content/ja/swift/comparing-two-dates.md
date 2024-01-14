---
title:    "Swift: 「二つの日付の比較」"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ
Swiftプログラミングにおいて、日付を比較することが重要な理由をお伝えします。

日付を比較することで、アプリケーションで使用されるさまざまなタイムゾーンのデータを正しく処理することができます。また、タスクの期限やイベントの日時を判断することができるので、ユーザーにとってより良い体験を提供することができます。

## 手順
日付を比較する方法をコード例とともにご紹介します。

```
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
 
let date1 = formatter.date(from: "01/01/2020")
let date2 = formatter.date(from: "01/01/2021")
 
if date1?.compare(date2!) == .orderedDescending {
    print("Date 1 is later than date 2")
} else if date1?.compare(date2!) == .orderedAscending {
    print("Date 1 is earlier than date 2")
} else {
    print("The dates are the same")
}
```

出力結果:
```
Date 1 is earlier than date 2
```

## 深堀り
日付を比較する方法についてさらに詳しく解説します。

Swiftでは、`Date`型の`compare`メソッドを使用して日付を比較することができます。このメソッドでは、日付が同じ場合は`orderedSame`、比較した日付より後の場合は`orderedDescending`、比較した日付より前の場合は`orderedAscending`という結果が返されます。

また、`Date`型を比較する際には、タイムゾーンの影響を受けないように注意する必要があります。日付を比較する前に、必要に応じてタイムゾーンを設定することが重要です。

## 参考リンク
- [Swift公式ドキュメント：Date](https://developer.apple.com/documentation/foundation/date)
- [日付および暦法の概要（Swift）](https://swift.ctolib.com/apple-docs-swift-weekly-v5-doc-cn-alloyteam-com-developer-apple-com-documentation-foundation-date.html)
- [日付や時間の表示方法を簡単にする標準ライブラリの使い方](https://qiita.com/KosukeMaeda/items/87813b46f78520dcd528)

## 参考になるリンク
- [SwiftのDate関連メソッド](https://qiita.com/jinwind/items/e1efb4f190e7efddd107)
- [日付の比較を実装しよう](https://academy.realm.io/jp/posts/making-a-calendar-app-for-ios/#toc-1)
- [日付とカレンダー](https://www.appcoda.com/swift-date-calendar/)
- [タイムゾーンを正しく処理する方法](https://dev.classmethod.jp/articles/working-with-dates-and-time-zones-on-ios/)

---
**参考文献：**

若林太, 堀井陽介, 遠藤武志, 岡野友将. (2016). "*Swiftポケットリファレンス*. "オーム社.