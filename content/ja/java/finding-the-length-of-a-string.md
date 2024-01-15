---
title:                "文字列の長さを見つける"
html_title:           "Java: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに関わる理由は、プログラムで処理する必要があるテキストの長さを知るためです。例えば、パスワードの長さをチェックする際には、文字列の長さを把握することが重要です。

## 使い方

文字列の長さを求める方法はいくつかあります。まずは、`String`クラスの`length()`メソッドを使う方法を見てみましょう。

```Java
String text = "Hello World!";
int length = text.length();
System.out.println("The length of the string is: " + length);

// Output: The length of the string is: 12
```

また、`String`クラスから派生した`StringBuilder`クラスを使って文字列を操作する場合にも、同じ`length()`メソッドが使えます。

```Java
StringBuilder sb = new StringBuilder("Hello World!");
int length = sb.length();
System.out.println("The length of the string is: " + length);

// Output: The length of the string is: 12
```

最後に、`String`クラスの`toCharArray()`メソッドを使って、文字列を文字の配列に変換し、その配列の長さを求める方法もあります。

```Java
String text = "Hello World!";
char[] charArray = text.toCharArray();
int length = charArray.length;
System.out.println("The length of the string is: " + length);

// Output: The length of the string is: 12
```

## 深堀り

`String`クラスの`length()`メソッドは、内部的には文字列のキャッシュを使用しており、毎回計算を行わなくても、キャッシュされた値を返すようになっています。そのため、文字列の長さを複数回取得したい場合には、`length()`メソッドを使う方が効率的です。

また、`String`クラスの`length()`メソッドは、ユニコード文字列の場合でも正しく動作します。つまり、ユニコードのサロゲートペアに対応した文字列でも、適切な長さが返されます。

## この他にも見てみる

- [Java String Length Tutorial](https://www.baeldung.com/java-string-length)
- [How to Find the Length of a String in Java](https://www.tutorialspoint.com/java-program-to-find-the-length-of-a-string)