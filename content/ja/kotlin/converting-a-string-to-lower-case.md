---
title:                "小文字に文字列を変換する"
html_title:           "Kotlin: 小文字に文字列を変換する"
simple_title:         "小文字に文字列を変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何故

文字列を小文字に変換することの利点は、プログラムの柔軟性と使いやすさです。

## 方法

文字列を小文字に変換するには、`toLowerCase()`メソッドを使用します。このメソッドは、以下のように使用します。

```Kotlin
val str = "HAPPY BIRTHDAY"
val lowerCaseStr = str.toLowerCase() // 出力は "happy birthday"
```

このように、`toLowerCase()`メソッドはオリジナルの文字列を小文字に変換し、新しい文字列として返します。また、英字以外の文字も小文字に変換することができます。例えば、ドイツ語の文字 `Ü` は小文字に変換すると `ü` になります。

```Kotlin
val germanStr = "GÜNTER"
val lowerCaseGermanStr = germanStr.toLowerCase() // 出力は "günter"
```

## ディープダイブ

Javaの場合、`toLowerCase()`メソッドは`Locale`引数を受け取り、その言語の正しいルールで文字を小文字に変換します。一方、Kotlinではこの引数が省略されています。そのため、Kotlinの小文字変換メソッドは常にロケールに依存しない一貫性のある結果を返します。

また、Kotlinではオプションとして`LOCALE`引数も受け取ることができます。これを指定すると、指定したロケールに従って文字を小文字に変換します。

```Kotlin
val str = "NICE WEATHER"
val loc = Locale("fr", "FR")
val lowerCaseStr = str.toLowerCase(loc) //出力は "nice weather"（フランス語のロケールに従って変換）
```

## 関連リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [JavaでのString小文字変換の仕組み](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase(java.util.Locale))
- [KotlinでのString小文字変換の仕組み](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/.html#to-lower-case)