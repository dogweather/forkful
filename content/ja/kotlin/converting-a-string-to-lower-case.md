---
title:    "Kotlin: 「文字列を小文字に変換する」"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

今日私たちは、Kotlinで文字列を小文字に変換する方法について説明します。文字列を小文字に変換する必要があるのは、より柔軟な文字列の処理や比較検索がしたい場合です。また、データベースやAPIからのデータを正規化する必要がある場合もあります。

## 方法

文字列を小文字に変換するには、 `toLowerCase()` メソッドを使用します。このメソッドは、文字列を全て小文字に変換します。

```Kotlin
val name = "JULIA"
val lowerCaseName = name.toLowerCase()
println(lowerCaseName) // 出力: julia
```

このように、 `toLowerCase()` メソッドを使用することで、簡単に文字列を小文字に変換できます。

## 深堀り

文字列を小文字に変換する際には、文字コードの違いに注意が必要です。KotlinではデフォルトでUTF-16文字コードが使用されるため、文字列がASCII文字で構成されている場合でも、文字コードは異なることがあります。そのため、文字列を小文字に変換する際には、`toLowerCase()` メソッドではなく、 `toLowerCase(Locale.ENGLISH)` メソッドを使用することをお勧めします。

```Kotlin
val name = "JULIA"
val lowerCaseName = name.toLowerCase(Locale.ENGLISH)
println(lowerCaseName) // 出力: julia
```

また、文字列を小文字に変換する際には、元の文字列が変更されるのではなく、新しい小文字の文字列が返されることにも注意してください。

## 関連情報

参考リンク：
- https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
- https://stackoverflow.com/questions/39474114/string-to-lowercase-in-kotlin

## 参考リンク