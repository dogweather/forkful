---
title:                "部分文字列の抽出"
html_title:           "Kotlin: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列から部分文字列を抽出することに興味を持つ人は多いかもしれません。それは、与えられた文字列から必要な情報を取り出すためにとても便利だからです。

## 使い方

文字列から部分文字列を抽出するには、  ```Kotlin str.substring(startIndex, endIndex) ```というメソッドを使用します。例えば、文字列 ```Kotlin "Hello World" ```から、  ```Kotlin "World" ```を抽出する場合、次のようになります。

```Kotlin
val str = "Hello World"
val subStr = str.substring(6, 11)
println(subStr)
```

出力結果：
```
World
```

## 詳細な説明

文字列から部分文字列を抽出する方法についてさらに詳しく説明します。

### インデックス

部分文字列を取得する際、2つのパラメーターを指定する必要があります。1つ目のパラメーターは「開始位置」、2つ目のパラメーターは「終了位置」です。この位置は文字列のインデックスによって表され、最初の文字のインデックスは0から始まります。

例えば、文字列 ```Kotlin "Hello World" ```における、"H"のインデックスは0、"e"のインデックスは1となります。

### インデックスの省略

部分文字列を取得する際、2つ目のパラメーターである「終了位置」は省略することができます。省略した場合、開始位置から文字列の最後までを含む部分文字列が抽出されます。

例えば、文字列 ```Kotlin "Hello World" ```から、最初の5つの文字列を抽出するには次のようになります。

```Kotlin
val str = "Hello World"
val subStr = str.substring(0, 5)
println(subStr)
```

出力結果：
```
Hello
```

### 負のインデックス

部分文字列を取得する際、開始位置と終了位置に負のインデックスを指定することができます。負のインデックスとは、最後の文字から数えたインデックスのことを指します。例えば、最後の文字のインデックスは-1、最後から2番目の文字のインデックスは-2となります。

例えば、文字列 ```Kotlin "Hello World" ```から、最後の5つの文字を抽出するには次のようになります。

```Kotlin
val str = "Hello World"
val subStr = str.substring(-5)
println(subStr)
```

出力結果：
```
World
```

## 参考リンク

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String.substring() Method](https://www.javatpoint.com/kotlin-string-substring)