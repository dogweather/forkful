---
title:                "二つの日付を比較する"
html_title:           "Swift: 二つの日付を比較する"
simple_title:         "二つの日付を比較する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較する理由は、プログラミングではよくあるシナリオです。たとえば、ユーザーの誕生日と現在の日付を比較して、年齢を計算したい場合などが挙げられます。

## 方法

まず、2つの日付を比較したいときには、`Date`オブジェクトに変換する必要があります。次に、`Calendar`クラスを使用して、日付を比較することができます。

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd" // 日付フォーマットを設定

let date1 = dateFormatter.date(from: "1990/01/01") // タイムスタンプが生成される

let date2 = dateFormatter.date(from: "2020/01/01")
// 以下、2つの方法で日付を比較することができます

// 1つ目の方法：compareメソッドを使用する
if date1.compare(date2) == .orderedAscending {
    print("date1がdate2より前にあります。")
} else if date1.compare(date2) == .orderedSame {
    print("date1とdate2は同じです。")
} else {
    print("date1がdate2より後にあります。")
}

// 2つ目の方法：compareメソッドを使用しないバージョン
if date1 < date2 {
    print("date1がdate2より前にあります。")
} else if date1 == date2 {
    print("date1とdate2は同じです。")
} else {
    print("date1がdate2より後にあります。")
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
date1がdate2より前にあります。
```

## 詳細を深く探る

日付を比較するときには、いくつかの注意点があります。まず、日付のフォーマットは重要です。例えば、`yyyy/MM/dd`のようにスラッシュで区切られたフォーマットは、西暦のフォーマットとして一般的ですが、`yyyy-MM-dd`のようなハイフンで区切られたフォーマットは、ISO8601フォーマットと呼ばれる規格に従います。日付の比較を行う際には、日付のフォーマットが一致しているかどうかを確認することが重要です。

また、日付だけでなく、時間も比較したい場合は`Date`オブジェクトではなく、`DateTime`オブジェクトを使用する必要があります。日付と時刻を含むオブジェクトを比較するには、`compare`メソッドではなく、`compare(_:toGranularity:calendar:matchingPolicy:)`メソッドを使用します。

## See Also

- [Article in English about comparing dates in Swift](https://www.hackingwithswift.com/example-code/language/how-to-compare-dates)
- [Apple's documentation on Date and Time Programming Guide](https://developer.apple.com/documentation/foundation/date_and_time_programming_guide)