---
title:    "Kotlin: 部分文字列の抽出"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ：切り出しを行うことの重要性

プログラミングを行うためには、時には文字列から一部の情報を抜き出す必要があります。これは、特定のテキストデータを処理するために非常に便利です。Kotlinでは、サブストリングを簡単に抜き出すことができるため、コードを効率的に記述することができます。

## 解説：サブストリングの抜き出し方法

Kotlinでは、`substring()`メソッドを使用して文字列からサブストリングを抜き出すことができます。以下の例では、`substring()`メソッドを使用して、最初の3文字からなるサブストリングを抜き出し、その出力結果を表示しています。

```Kotlin
val name = "田中太郎"
val substring = name.substring(0, 3)
println(substring) // 出力結果：田中
```

また、特定の文字列を含む場合には、`substringAfter()`や`substringBefore()`メソッドを使用することで、その文字列を基準にサブストリングを抜き出すこともできます。以下の例では、`@`を基準にサブストリングを抜き出し、その出力結果を表示しています。

```Kotlin
val email = "tanaka_taro@example.com"
val substring = email.substringAfter("@")
println(substring) // 出力結果：example.com
```

## 深堀り：サブストリングの詳細

Kotlinでは、`substring()`メソッドを使用する際に、開始インデックスと終了インデックスの二つのパラメーターを指定する必要があります。開始インデックスは、抜き出したいサブストリングの最初の文字のインデックスを表し、終了インデックスは、抜き出したいサブストリングの最後の文字の次のインデックスを表します。また、省略することで、文字列の最初から最後までを抜き出すこともできます。

また、`substringAfter()`や`substringBefore()`メソッドを使用する際には、基準となる文字列を指定する必要があります。指定した文字列が複数回出現する場合には、最初に見つかった箇所を基準にサブストリングが抜き出されます。

## 参考：関連リンク

- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/tutorials/basic-syntax.html)
- [Kotlinプログラミング基礎](https://www.talentica.com/blogs/kotlin-programming-basics/)
- [Kotlin学習ガイド](https://www.tutorialspoint.com/kotlin/index.htm)

See Also（参考文献）： 
 - [Kotlinチュートリアル：Kotlinでサブストリングを抜き出す方法](https://www.callicoder.com/kotlin-substring/)
 - [Kotlinのsubstringメソッドを理解しよう](https://medium.com/@yagish306/kotlin%E3%81%AEsubstring%E3%83%A1%E3%82%BD%E3%83%83%E3%83%89%E3%82%92%E7%90%86%E8%A7%A3%E3%81%97%E3%82%88%E3%81%86-ab0b4c61f8ed)