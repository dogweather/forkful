---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？
現在の日付を取得するとは、システムの日付や時間に対するプログラムからの照会を意味します。これは、ログのタイムスタンプを作成したり、特定の時間に特定のアクションをトリガーしたりするためにプログラマーが行います。

## 方法：
Kotlinで現在の日付を取得する簡単な方法は、以下のコードを使用することです：

```Kotlin
import java.time.LocalDate

fun main() {
   val currentDate = LocalDate.now()
   println("現在の日付は：$currentDate")
}
```

このコードを実行すると、次のような出力が表示されます：

```Kotlin
現在の日付は：2022-03-10
```

## 深掘り：
### 歴史的な文脈：
`LocalDate`パッケージは、Java 8で導入され、Kotlinにも反映されました。これは、Javaが古い`java.util.Date`クラスから移行する一環として作成されました。

### 代替方法：
あるいは、古い`Date`クラスを使用して現在の日付を取得することもできます。

```Kotlin
import java.util.Date

fun main() {
   val currentDate = Date()
   println("現在の日付は：$currentDate")
}
```

だけど、`java.util.Date`は古いですから、新たなJava8の日付APIを使うメリットが多いです。

### 実装の詳細：
`LocalDate.now()`はシステムクロックに基づいて現在の日付を提供します。デフォルトでは、システムデフォルトのタイムゾーンを使用します。

## 参照：
- [Kotlinの公式ドキュメンテーション](https://kotlinlang.org/docs/reference/)
- [Java 8の日付/時間API](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)