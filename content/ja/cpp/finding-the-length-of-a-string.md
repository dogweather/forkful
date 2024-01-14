---
title:                "C++: Japanese: 文字列の長さを見つける"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーの中には、文字列の長さを取得することが必要になることがあるかもしれません。例えば、文字列に何らかの処理を施す前にその長さを確認する必要がある場合や、文字列の一部を切り取る際に長さを考慮する必要がある場合などです。このプログラミングのハウツーでは、文字列の長さを取得する方法について紹介します。

## 方法

まずは、文字列の長さを取得する方法を簡単なコード例で説明します。以下のサンプルコードでは、"Hello, World!"という文字列の長さを取得して表示するプログラムを紹介します。

```C++
#include <iostream>
#include <string>

int main() {
  std::string str = "Hello, World!";
  int len = str.length();
  std::cout << "String length = " << len << std::endl;
  return 0;
}
```
**出力:**
String length = 13

この例では、まず`<string>`ヘッダーファイルをインクルードしています。これは、文字列を扱うために必要なヘッダーファイルです。そして、`str.length()`を使って文字列の長さを取得し、`len`という変数に代入しています。最後に、`std::cout`を使って長さを表示します。

C++では、文字列の長さを取得するためにさまざまな方法があります。例えば、`str.size()`や`str.capacity()`を使うこともできます。しかし、一般的には`str.length()`を使うことが推奨されています。

## ディープダイブ

文字列の長さを取得する方法について詳しく見ていきましょう。`length()`メソッドは、文字列オブジェクトに対して定義されているメソッドであり、その文字列の長さを返します。このメソッドは、文字列の長さを取得する際に重要な役割を果たしています。

また、C++の文字列は`std::string`というクラスで表されています。これは標準ライブラリとして提供されており、様々な便利なメソッドが用意されています。そのため、一般的な文字列処理は`std::string`クラスを使うことで簡単に行うことができます。

さらに、文字列の長さを取得する際にはUTF-8と呼ばれる文字コードを考慮する必要があります。UTF-8では、1つの文字が複数のバイトで表されるため、単純にバイト数を数えるだけでは正確な文字数が得られません。そのため、C++では`length()`メソッドを使って正確な文字列の長さを取得することができます。

## それでは、さっそく試してみましょう！

- [C++ string length() reference](https://www.cplusplus.com/reference/string/string/length/)
- [UTF-8とは](https://ja.wikipedia.org/wiki/UTF-8)
- [Markdownの使い方](https://qiita.com/tbpgr/items/989c6badefff69377da7)

## 関連記事

- [C++ 文字列の比較方法](https://example.com/blog/cpp-string-compare)
- [C++ 文字列の置換方法](https://example.com/blog/cpp-string-replace)