---
title:    "Kotlin: 日付を文字列に変換する"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
さあ、皆さんは日付を文字列に変換する必要があることがあるでしょうか？多くのプログラマーは、日付を文字列に変換する必要がある場合があります。例えば、アプリケーションで日付を表示する必要がある場合や、日付をデータベースに保存する必要がある場合などです。この記事では、Kotlinで日付を文字列に変換する方法を紹介します。

## 方法
まず、Kotlinの「Date」クラスを使って日付を作成します。「Date」クラスは、指定した年、月、日を持つ日付オブジェクトを作成するために使用します。例えば、2021年7月26日を表す日付オブジェクトを作成するには、次のようにします。
```
val date = Date(2021, 7, 26)
```
次に、日付を文字列に変換するには、Kotlinの「SimpleDateFormat」クラスを使用します。このクラスには、日付を指定したフォーマットに変換するための便利なメソッドがあります。例えば、次のように「yyyy/MM/dd」フォーマットに変換することができます。
```
val dateString = SimpleDateFormat("yyyy/MM/dd").format(date)
```
以下は、完全なコードと出力の例です。
```
fun main() {
    val date = Date(2021, 7, 26)
    val dateString = SimpleDateFormat("yyyy/MM/dd").format(date)
    println(dateString)
}
```
出力：
```
2021/07/26
```

## 深堀り
「SimpleDateFormat」クラスでは、さまざまなフォーマットオプションを使用して日付を文字列に変換することができます。例えば、年や月、日の他にも、曜日を表す「E」や、24時間表示の時刻を表す「HH」などのオプションがあります。また、日付を時間と結合したり、ロケール（地域や言語）を指定したりすることもできます。詳細な情報は、公式ドキュメントを参照してください。

## 参考
- [Kotlin公式ドキュメント - Date](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/index.html)
- [Kotlin公式ドキュメント - SimpleDateFormat](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)
- [公式ドキュメント - 日付と時刻の書式指定](https://docs.oracle.com/javase/jp/8/docs/api/java/text/SimpleDateFormat.html)