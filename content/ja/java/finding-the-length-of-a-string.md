---
title:    "Java: 文字列の長さを見つける"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ

文字列の長さを求めることに関心を持つか、その理由は何でしょうか？文字列の長さを知ることは、プログラミングで非常に便利です。例えば、文字列を処理する際に、その長さを事前に知っておくことで、メモリの使用量を最適化することができます。また、文字列の長さを知ることで、文字列を正しく表示したり、文字列の一部を取り出したりすることができます。つまり、文字列の長さを求めることは、プログラミングにおいて非常に重要なスキルです。

# 方法

文字列の長さを求める方法はいくつかありますが、ここではJavaでの実装方法を紹介します。まず、`String`クラスの`length()`メソッドを使うことで、文字列の長さを求めることができます。例えば、以下のコードを実行すると、文字列「Hello World」の長さである`11`が出力されます。

```Java
String str = "Hello World";
int length = str.length();
System.out.println(length);
```

また、文字列を扱う際には、引用符（「"」や「'」）を除いた実際の文字数を知りたい場合があります。その場合は、`String`クラスの`codePointCount()`メソッドを使うことで、単語の長さが分かります。例えば、以下のコードを実行すると、文字列「こんにちは」の文字数である`5`が出力されます。

```Java
String str = "こんにちは";
int length = str.codePointCount(0, str.length());
System.out.println(length);
```

# ディープダイブ

文字列の長さを求める方法をより詳しく調べると、実際には文字コードという概念に基づいて長さが計算されることが分かります。文字コードとは、コンピュータが文字を表現するためのコードのことです。文字コードには様々な種類がありますが、JavaではUnicodeという文字コードが採用されています。このUnicodeという文字コードでは、英数字などの一部の文字は1つのbyteで表現されますが、他の言語の文字（例えば日本語）は2つのbyteで表されるため、文字数を計算する際にはこの点に注意する必要があります。

# 参考リンク

- [Java String length() method](https://www.javatpoint.com/java-string-length)
- [Java String codePointCount() method](https://www.javatpoint.com/java-string-codepointcount)