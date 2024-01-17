---
title:                "文字列の抽出"
html_title:           "C++: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何かい？
抽出サブストリングとは何か？それは、文字列の一部を取り出し、それを単独で使用することを指します。プログラマーは、特定の目的や条件に合わせて、文字列の一部を抽出することで、より効率的なコーディングを行うことができます。

## 方法：
C++でサブストリングを抽出する方法を学ぶには、単純なコード例を見てみましょう。例えば、次のようなコードを使用することで、文字列の一部を抽出することができます。

```
// 元の文字列
string originalString = "こんにちは、私はプログラマーです。";

// サブストリングを抽出する
string subString = originalString.substr(8, 10);

// 出力結果：「プログラマー」
cout << subString << endl;
```

この場合、```substr()```関数を使用し、原文の8番目の文字から10文字を抽出しています。結果として、出力されるのは「プログラマー」という部分文字列です。

## 深く掘り下げる：
サブストリングの歴史的背景を知りたい方は、この機能が最初に登場したのは1979年のFORTRAN言語であることを知っておくと良いでしょう。また、C++以外にも同様の機能を備えた言語として、JavaやPythonなどがあります。

サブストリングを抽出する他の方法としては、正規表現を使用するという方法もあります。しかし、C++には標準の正規表現ライブラリがないため、ライブラリを追加する必要があります。

C++におけるサブストリングの実装については、実際には一連のポインター操作を通じて行われており、文字列のコピーを行う際には注意が必要です。

## 関連情報：
- [C++ string::substr() Document](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [History and Evolution of Substrings](https://www.coin-or.org/CppAD/Doc/cppad.htm#stdstring_substring)