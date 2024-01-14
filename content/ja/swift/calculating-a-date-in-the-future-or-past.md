---
title:                "Swift: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Why

日付を計算するのになぜ時間を費やすのでしょうか？簡単に言うと、人々は過去や未来の特定の日付を知りたいと思うことがあります。例えば、誕生日や記念日の計画を立てるため、特定の日付に何週間後が何月か知りたいと思うかもしれません。Swiftでは、日付の計算を非常に簡単にする方法があります。

##How To

まず、日付を計算するには、FoundationフレームワークからDateクラスをインポートする必要があります。その後、Dateオブジェクトを作成し、DateクラスのメソッドであるaddingTimeIntervalを使用して、過去や未来の特定の日数を追加することができます。例えば、今日の日付から3日後の日付を計算するには、以下のようなコードを記述します。

```
Swift
import Foundation

let today = Date()
let futureDate = today.addingTimeInterval(3 * 24 * 60 * 60) // 3日後を計算
print(futureDate) // 結果例：2019-04-04 01:23:45 +0000
```

24 * 60 * 60は1日の秒数を表しているため、3を掛けることで3日後を表すことができます。

同様に、過去の日付を計算することもできます。例えば、1か月前の日付を計算するには、マイナスの値を使用します。

```
Swift
let pastDate = today.addingTimeInterval(-30 * 24 * 60 * 60) // 1か月前を計算
print(pastDate) // 結果例：2019-02-31 01:23:45 +0000
```

上記のコードでは、addigTimeIntervalメソッドの引数に-30を指定しています。これにより、現在の日付から1か月前を計算することができます。

##Deep Dive

Dateクラスには、日付を計算するためのさまざまなメソッドがあります。先ほど使用したaddingTimeIntervalのほかにも、addingDays、addingMonths、addingYearsなどのメソッドがあります。これらのメソッドを使用すると、日付の加算だけでなく、減算も簡単に行うことができます。

さらに、DateIntervalクラスを使用すると、2つの日付の間の差を計算することも可能です。これを利用することで、日付の比較を行い、過去や未来の日付を特定することができます。

詳細な情報やサンプルコードは、公式のドキュメントを参照することができます。

##See Also

- [Date Class | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- [Date Interval Class | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateinterval)