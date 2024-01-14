---
title:    "Kotlin: 将来や過去の日付を計算する"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ
未来や過去の日付を計算することに取り組む理由は、特定の日付を基準に、期限や締め切り、イベントの日程などを確認したいためです。

# 使い方
```Kotlin
// 今日の日付を取得
val today = LocalDate.now()

// 1年後の日付を計算して出力
val oneYearFromNow = today.plusYears(1)
println("1年後の日付: $oneYearFromNow")

// 1ヶ月前の日付を計算して出力
val oneMonthAgo = today.minusMonths(1)
println("1ヶ月前の日付: $oneMonthAgo")

// 特定の日付からの経過日数を計算
val christmas = LocalDate.of(2021, 12, 25)
val daysUntilChristmas = today.until(christmas, ChronoUnit.DAYS)
println("今日からクリスマスまであと$daysUntilChristmas日")
```

# 詳細
Kotlinの日付を扱うライブラリである"java.time"パッケージのクラスを使用することで、簡単に未来や過去の日付を計算することができます。

例えば、"plusYears"や"minusMonths"などのメソッドを使用することで、指定した期間のずれた日付を計算することができます。また、"until"メソッドを使用することで、特定の日付と今日の日付の間に何日間あるかを取得することができます。

# 参考リンク
[Java Time APIドキュメンテーション](https://docs.oracle.com/javase/jp/8/docs/api/java/time/package-summary.html)

[kotlinx-datetimeドキュメンテーション](https://kotlinlang.org/docs/datetime.html)

# 関連リンク
[Java Time APIの使い方まとめ](https://techacademy.jp/magazine/24060)

[Kotlinで日付を扱う方法](https://qiita.com/nakaken0629/items/22107ce7c13b75efbc87)