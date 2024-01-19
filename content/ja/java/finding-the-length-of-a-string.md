---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列の長さを見つけるとは、文字列が何文字かを数えることです。それは、文字列の終わりを判定したり、バッファの大きさを設定したりする際にプログラマーが行う作業です。

## どうやって：
こんなに簡単にできます：
```Java
public class Main {
    public static void main(String[] args) {
        String str = "Programming in Java";
        System.out.println("The length of the string is: " + str.length());
    }
}
```
このコードを実行すると、以下のような結果が表示されます。
```
The length of the string is: 19
```
文字列"Programming in Java"は19文字ですと表示されます。

## 深掘り：
文字列の長さを見つけるためのメソッドは、Javaが最初にリリースされた1995年から存在します。文字列クラス的にはこれが最も基本的な操作です。
言語によっては、文字列の長さを格納するメモリ領域を持つものもありますが、Javaでは毎回計算する方式がとられています。`strlen`というC言語の関数に相当します。

また、Javaでは`.length()`メソッドの他にも`.isEmpty()`メソッドを使って文字列が空であるかどうかを確認することもできます。

## 参考文献：
1. [Oracle Java Documentation on String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
2. [Wikipedia Article on Java's String class](https://en.wikipedia.org/wiki/Java_performance)