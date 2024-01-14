---
title:    "Java: 文字列の結合"
keywords: ["Java"]
---

{{< edit_this_page >}}

# なぜ文字列を連結するのか

Javaプログラミングで重要な概念の1つは、文字列を連結することです。文字列を連結することは、簡単なタスクのように見えますが、実際には非常に有用な方法です。この記事では、なぜ文字列を連結するのか、そしてどのようにするのかを紹介します。

## 方法

文字列を連結するには、Javaで組み込みのメソッドである`concat()`を使用します。このメソッドは、2つの文字列を結合し、新しい文字列を返します。以下の例では、`concat()`を使用して2つの文字列を結合し、それを新しい変数に代入しています。

```Java
String str1 = "Hello";
String str2 = "World";

String combinedString = str1.concat(str2);

System.out.println(combinedString);
```

出力は次のようになります。

```
HelloWorld
```

また、`+`演算子を使用することでも文字列を連結することができます。

```Java
String str1 = "Hello";
String str2 = "World";

String combinedString = str1 + str2;

System.out.println(combinedString);
```

出力は同じく`HelloWorld`になります。

## 深く掘り下げる

文字列を連結する方法は単純ですが、そのバックグラウンドにはいくつかの重要な概念があります。Javaでは、文字列は不変（immutable）であるため、文字列を変更する場合は常に新しい文字列を作成する必要があります。そのため、`concat()`や`+`演算子を使用して文字列を連結すると、実際には新しい文字列オブジェクトが生成され、元の文字列は変更されません。このように、Javaではメモリの使用に注意する必要があります。

また、文字列を連結する際は、大量の文字列を結合するとパフォーマンスが低下する可能性があります。その場合、より効率的に文字列を連結するために、`StringBuilder`クラスを使用することをお勧めします。`StringBuilder`は可変（mutable）な文字列を扱うため、メモリの消費が少なく高速な文字列操作が可能です。

## 関連記事

- [Java String Concatenation](https://www.geeksforgeeks.org/java-string-concatenation/)
- [StringBuilder Class in Java](https://www.baeldung.com/java-string-builder)