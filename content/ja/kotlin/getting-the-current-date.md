---
title:    "Kotlin: 「現在の日付を取得する」"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ？
現在の年月日を取得する理由は、アプリケーションやウェブサイトでタイムスタンプを使用する場合や、特定の機能を制限するための期限日を設定する場合などがあります。

## 方法: 
```Kotlin
// 現在の日付を取得する 
val date = LocalDate.now()
println("今日の日付は ${date}")

// 現在の時刻を取得する
val time = LocalTime.now()
println("現在の時刻は ${time}")

// 現在の日時を取得する
val dateTime = LocalDateTime.now()
println("現在の日時は ${dateTime}")

// カスタムフォーマットで現在の日時を取得する
val formatter = DateTimeFormatter.ofPattern("yyyy年MM月dd日 HH:mm:ss")
val customDateTime = LocalDateTime.now().format(formatter)
println("現在の日時は ${customDateTime}")
```

出力:

```
今日の日付は 2021-01-01
現在の時刻は 18:30:00.123456
現在の日時は 2021-01-01T18:30:00.123456
現在の日時は 2021年01月01日 18:30:00
```

## ディープダイブ:
現在の日時を取得するには、Kotlinの`LocalDate`、`LocalTime`、`LocalDateTime`クラスを使用します。これらは、日付や時刻をカスタマイズするための各種メソッドを提供しています。また、`DateTimeFormatter`クラスを使用することで、日付や時刻のフォーマットをカスタマイズすることができます。詳細な使い方については、公式ドキュメントを参照してください。

## おわり:
ここでは、Kotlinで現在の日時を取得する方法についてご紹介しました。タイムスタンプや期限日を設定する際に必要な知識であるため、覚えておくと便利です。Kotlinを使って日時を取得する際は、ぜひこの方法を使用してみてください。

## 関連リンク:
- [Java Time API | Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin | Accessing Current Date and Time | GeeksforGeeks](https://www.geeksforgeeks.org/kotlin-accessing-current-date-and-time/)
- [Kotlin | Date Time Formatting | javatpoint](https://www.javatpoint.com/kotlin-date-time-formatting)