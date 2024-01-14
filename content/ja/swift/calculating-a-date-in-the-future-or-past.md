---
title:    "Swift: 将来または過去の日付を計算する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# なぜ

将来や過去の日付を計算するのにどのような理由があるのか、1-2文で説明します。

# 使い方

「```Swift ... ```」のコードブロック内に、コーディングの例と出力のサンプルを記載します。

- 例1：将来の日付を計算する

```Swift
// カレンダーを作成
var calendar = Calendar.current

// 今日の日付を取得
let today = Date()

// 10日後の日付を計算
let futureDate = calendar.date(byAdding: .day, value: 10, to: today)

// 結果を出力
print(futureDate) // 10日後の日付が出力される
```

- 例2：過去の日付を計算する

```Swift
// カレンダーを作成
var calendar = Calendar.current

// 今日の日付を取得
let today = Date()

// 10日前の日付を計算
let pastDate = calendar.date(byAdding: .day, value: -10, to: today)

// 結果を出力
print(pastDate) // 10日前の日付が出力される
```

# 深堀り

今度は、より深く日付の計算について説明します。この方法を使用すると、年、月、日、時間、分、秒など、さまざまな単位で日付を計算することができます。

- 年の計算方法

```Swift
// カレンダーを作成
var calendar = Calendar.current

// 今日の日付を取得
let today = Date()

// 2年後の日付を計算
let futureDate = calendar.date(byAdding: .year, value: 2, to: today)

// 結果を出力
print(futureDate) // 2年後の日付が出力される
```

- 月の計算方法

```Swift
// カレンダーを作成
var calendar = Calendar.current

// 今日の日付を取得
let today = Date()

// 3ヶ月前の日付を計算
let pastDate = calendar.date(byAdding: .month, value: -3, to: today)

// 結果を出力
print(pastDate) // 3ヶ月前の日付が出力される
```

- 日の計算方法

```Swift
// カレンダーを作成
var calendar = Calendar.current

// 今日の日付を取得
let today = Date()

// 15日後の日付を計算
let futureDate = calendar.date(byAdding: .day, value: 15, to: today)

// 結果を出力
print(futureDate) // 15日後の日付が出力される
```

これらの例を参考にしながら、さまざまな方法で日付を計算することができます。また、上記のコードで使用した`.year`、`.month`、`.day`の他にも`.minute`や`.second`など、さまざまなオプションがありますので、ぜひ試してみてください。

# 参考リンク

次のリンクは、日付を計算する際に参考になるサイトです。

- **Apple公式ドキュメント**：[計算プロパティ](https://developer.apple.com/documentation/foundation/calendar/2293753-date)

- **Stack Overflow**：[How to get current date into a specific format](https://stackoverflow.com/questions/4557532/how-to-get-current-date-into-a-specific-format)