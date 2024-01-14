---
title:                "Kotlin: 日付の取得方法"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
現在の日付を取得することに関わる理由を説明するための1〜2文です。

## 方法
「```Kotlin ...```」コードブロック内のコーディング例とサンプル出力を含みます。

```Kotlin
fun main() {
    // 現在の日付を取得する
    val today = LocalDate.now()
    // 年、月、日を表示する
    println("今日の日付は ${today.year}年 ${today.monthValue}月 ${today.dayOfMonth}日 です。")
}
```

**出力:**
```
今日の日付は 2021年 6月 20日 です。
```

## 詳細を深める
現在の日付を取得する方法についての詳細な情報です。

### 日付オブジェクト
日付を表すためには、Kotlinの`LocalDate`クラスを使用します。このクラスには、日付を操作するための便利なメソッドがたくさんあります。

### フォーマット
`LocalDate`クラスでは、日付を様々なフォーマットで表示することができます。例えば、日付を「YYYY/MM/DD」の形式で表示するには、`DateTimeFormatter`クラスを使用します。

```Kotlin
val formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")
val today = LocalDate.now()
// 日付を指定したフォーマットで表示する
println("今日の日付は ${today.format(formatter)} です。")
```

**出力:**
```
今日の日付は 2021/06/20 です。
```

## 参考リンク
- [Kotlinの日付と時刻](https://kotlinlang.org/docs/datetime.html)
- [Java 8の日付と時刻APIの使用](https://www.oracle.com/java/technologies/javase8-64-bit-downloads.html)
- [日付と時刻のフォーマットについてのチュートリアル](https://www.codevoila.com/post/85/java-working-with-datetime-type-formatting-datatime)