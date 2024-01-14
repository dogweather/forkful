---
title:    "Kotlin: 「日付を文字列に変換する方法」"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ
ある日、プログラミングをしていて、日付を文字列に変換する必要が出てきたらどうしますか？そんな時に、コーディングにおいて日付を文字列に変換する方法を知っていることは非常に重要です。この記事では、Kotlinを使って日付を文字列に変換する方法をご紹介します。

## ハウツー
### SimpleDateFormatを使う方法
```Kotlin
val currentDate = Date()

// 日付を文字列に変換するためのフォーマットを設定
val formatter = SimpleDateFormat("dd/MM/yyyy")

// 日付を文字列に変換
val dateString = formatter.format(currentDate)

// 出力：04/10/2021
println(dateString)
```

### DateFormatを使う方法
```Kotlin
val currentDate = Date()

// 日付のフォーマットを指定してDateFormatオブジェクトを生成
val dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM, Locale.getDefault())

// 日付を文字列に変換
val dateString = dateFormat.format(currentDate)

// 出力：4 Oct, 2021
println(dateString)
```

### DateTimeFormatterを使う方法
```Kotlin
val currentDate = LocalDate.now()

// 日付のフォーマットを指定してDateTimeFormatterオブジェクトを生成
val dateFormatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")

// 日付を文字列に変換
val dateString = currentDate.format(dateFormatter)

// 出力：2021/10/04
println(dateString)
```

## ディープダイブ
日付を文字列に変換する方法は様々ありますが、Kotlinでは主にSimpleDateFormat、DateFormat、そしてDateTimeFormatterの3つの方法を使うことができます。それぞれの方法には長所と短所があり、使用する際にはどの方法が最適かを良く考える必要があります。

### SimpleDateFormat
この方法は最も古い方法であり、JDK 1.1から存在しています。デフォルトでは非スレッドセーフであるため、複数のスレッドから同時に使用する場合には注意が必要です。また、フォーマットが柔軟であり、カスタマイズすることができますが、ミスマッチの可能性があるといったデメリットもあります。

### DateFormat
JDK 1.1から使用できるようになったこの方法は、SimpleDateFormatの改良版ともいえます。フォーマットを指定することなく、地域や言語に応じて適切なフォーマットを自動的に選択してくれるため、便利です。また、スレッドセーフであるため、複数のスレッドから同時に使用することができます。

### DateTimeFormatter
Java 8から追加されたこの方法は、immutableなオブジェクトであり、スレッドセーフです。また、パターンを用いた柔軟なフォーマットが可能であり、新しいAPIを使用することで柔軟性が向上しました。

## See Also
- [Kotlin日付操作の基本](https://codezine.jp/article/detail/12484)
- [Kotlinで日付・時刻を操作する方法](https://www.casleyconsulting.co.jp/blog/engineer/537/)
- [SimpleDateFormat Class](https://developer.android.com/reference/java/text/SimpleDateFormat)
- [DateTimeFormatter Class](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)