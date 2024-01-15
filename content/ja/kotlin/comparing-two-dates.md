---
title:                "「二つの日付を比較する」"
html_title:           "Kotlin: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

何年か前、カレンダーが手元にない状況で今日の日付と誕生日を確認する必要がありました。そこで、自分でプログラムすることにしました。Kotlinを使って、今日の日付と任意の日付を比較する方法を学んでみましょう。

## 使い方

まず、Kotlinの標準ライブラリにある`LocalDate`クラスをインポートします。

```Kotlin
import java.time.LocalDate
```

今日の日付を取得し、`LocalDate`オブジェクトとして定義します。

```Kotlin
val today = LocalDate.now()
```

次に、比較する任意の日付を定義しましょう。

```Kotlin
val birthday = LocalDate.of(1995, 7, 15)
```

そして、`LocalDate`クラスにある`isAfter()`メソッドを使って、今日の日付が誕生日より後の日付かどうかを判定します。

```Kotlin
if(today.isAfter(birthday)) {
    println("今日は誕生日よりも後の日付です！")
} else {
    println("今日は誕生日よりも前の日付です。")
}
```

以上のコードを実行すると、今日が誕生日よりも後の日付であるかどうかを判定してメッセージを出力します。

```
今日は誕生日よりも後の日付です！
```

## 深堀り

`LocalDate`クラスは、日付の比較以外にもさまざまな便利なメソッドを提供しています。例えば、`isEqual()`メソッドを使うことで、2つの日付が完全に同じかどうかを判定することができます。

また、`plusDays()`や`minusDays()`メソッドを使うことで、任意の日数を加算したり減算したりすることもできます。

さらに、`LocalDate`クラスには西暦や月の長さに関する情報を取得できるメソッドも備わっており、より柔軟な日付の操作が可能です。

## 参考リンク

- [Kotlin 公式ドキュメント](https://kotlinlang.org/docs/reference/datetime.html)
- [Javaのjava.util.dateとJava8のLocalDateの違いについて知ろう](https://qiita.com/yukieen-ca/items/f9fb8e426e72ed7ef73e)