---
title:                "文字列を小文字に変換する"
html_title:           "C++: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
  文字列を小文字に変換する作業について学ぶ必要性について説明します。

コンピューターで文字列を扱う際、大文字と小文字は区別されています。そのため、大小文字を無視して文字列を扱いたい場合、文字列をすべて小文字に変換する必要があります。

## 方法
  ```C++
  #include <iostream>
  #include <string>
  #include <algorithm> // std::transform

  using namespace std;

  // 大文字を小文字に変換する関数
  string toLowerCase(string str) {

    // 文字列の一文字ずつ小文字に変換する
    transform(str.begin(), str.end(), str.begin(), ::tolower);

    return str;
  }

  int main() {
    string text = "Hello, WORLD!";

    // 出力: hello, world!
    cout << toLowerCase(text) << endl;

    return 0;
  }
  ```
  上記のコードは、C++で文字列を小文字に変換する方法の一例です。ここでは、文字列を小文字に変換するために `transform()` 関数を使用しています。この関数は、文字列の各文字に対して指定した関数を適用することができます。

  また、C++標準ライブラリには `toupper()` や `tolower()` といった大文字と小文字を変換する関数も用意されています。これらの関数を使用することでも文字列を小文字に変換することができます。

## 深堀り
  文字列を小文字に変換する際、英語以外の言語を扱う場合には注意が必要です。C++の標準ライブラリは、UTF-8エンコーディングに対応しており、英語のアルファベットのみを扱う場合には問題ありません。しかし、他の言語のアルファベットや記号を含む場合、 `tolower()` や `toupper()` 関数では期待した結果が得られない可能性があります。

  そのため、多言語に対応したライブラリを使用するか、独自にアルファベットや記号を変換する関数を実装する必要があります。

## See Also
  - [ctype.h ヘッダー - cppreference.com](https://ja.cppreference.com/w/cpp/header/cctype)
  - [std::transform - cppreference.com](https://ja.cppreference.com/w/cpp/algorithm/transform)