---
title:                "Kotlin: 文字列の抽出"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# なぜ：文字列を抽出する理由

文字列を扱う場合、時には特定の部分だけを抽出する必要があります。例えば、ある日付を表す文字列から「年」や「月」の部分だけを抽出したい場合や、名前のような長い文字列から特定の単語を取り出したい場合があります。このような場面で、文字列の抽出を行うことで、必要な情報を簡単に取得することができます。

# 抽出方法

```Kotlin
// 文字列から特定の部分を抽出する方法
val date = "2020/01/01"

// 年を抽出する場合
val year = date.substring(0,4)
println(year) // 出力結果： 2020

// 月を抽出する場合
val month = date.substring(5,7)
println(month) // 出力結果： 01

// 名前から特定の単語を抽出する場合
val fullName = "山田太郎"
val lastName = fullName.substring(0,2)
println(lastName) // 出力結果： 山田
```

上記の例では、substring()メソッドを使用して文字列から部分を抽出しています。このメソッドは、2つの引数を受け取り、最初の引数から始まり、2つ目の引数のインデックスより前までの文字列を抽出します。また、substring()メソッドを使用する際には、文字列のインデックスを指定する必要があります。日付の場合、年は0番目の位置から始まり、/の位置で区切られているため、その位置を指定することで抽出することができます。

# 深堀り

Kotlinでは、substring()メソッドの他にも、文字列から部分を抽出するための様々なメソッドが用意されています。例えば、substringAfter()やsubstringBefore()メソッドを使用することで、特定の文字列の前後の部分を抽出することができます。また、正規表現を使用して、パターンにマッチする部分を抽出することも可能です。さらに、文字列を文字列配列に変換して、特定の部分だけを取得することもできます。

# 関連リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/strings.html)
- [substring()メソッドの使い方](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [正規表現を使用した文字列の抽出方法](https://www.baeldung.com/kotlin/extract-regex)
- [文字列配列を使用した部分の取得方法](https://www.baeldung.com/kotlin/substring-arrays)