---
title:                "文字列の連結"
html_title:           "Java: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何をするのか？
文字列を連結することは、複数の文字列を一つに結合することを意味します。プログラマーがこれをするのは、文字列を組み立てたり、出力を整形したりするためです。

## 方法：
```Java
public class ConcatenationExample {

    public static void main(String[] args) {

        // 例１：文字列の連結
        String str1 = "Hello";
        String str2 = "world";
        String concatStr = str1 + str2;
        System.out.println(concatStr); //Hello world

        // 例２：数値と文字列の連結
        String numStr = "1 + 2 = ";
        int num1 = 1;
        int num2 = 2;
        String equation = numStr + (num1 + num2);
        System.out.println(equation); //1 + 2 = 3
    }
}
```

## 詳細を掘り下げる:
文字列の連結は、プログラミング言語の歴史が古く、基本的な機能の一つです。代替手段として、StringBuilderクラスがあります。しかし、文字列の数が少ない場合は、単純に連結演算子を使用する方が簡単です。文字列の連結は、メモリを一時的に消費するため、処理が重くなる可能性があります。

## 関連情報:
- [Javaで文字列を連結する方法](https://www.javatpoint.com/java-string-concatenation)
- [StringBuilderクラスの使用法](https://www.geeksforgeeks.org/string-vs-stringbuilder-vs-stringbuffer-in-java/)