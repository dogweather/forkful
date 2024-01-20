---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結とは、2つ以上の文字列を1つに結合することです。プログラムにおいて、これは情報を整形したり、ユーザーに提示するためによく行われます。

## 使い方：

Kotlinでは、「+」演算子を使って文字列を連結できます。または、 `String.concat` メソッドを使うことも可能です。

```Kotlin
val str1 = "Hello, "
val str2 = "World"
val str3 = str1 + str2
println(str3)
```
実行すると、出力は次のようになります：

``` 
Hello, World
```
また、`String.concat`メソッドを使う例：
```Kotlin
val str1 = "Hello, "
val str2 = "World"
val str3 = str1.concat(str2)
println(str3)
```
出力は以下の通り：

```
Hello, World
```

## ディープダイブ：

文字列の連結は非常に古い概念で、プログラミング言語が誕生した当初から存在しています。Kotlinにおいては、`x + y`や`x.concat(y)`以外に、sprintf関数やStringBuilderクラスを使って文字列連結を実現することもできます。しかし、通常、 `+` 演算子または `concat` メソッドが使用されます。

## 参考資料：

[公式ドキュメンテーション：String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)  
[公式ドキュメンテーション：StringBuilderクラス](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)