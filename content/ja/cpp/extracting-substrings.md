---
title:                "C++: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

プログラミング言語C ++を学習するのにあたり、文字列操作は重要なスキルです。その中でも、サブストリング（部分文字列）を抽出することはとても便利です。サブストリングを抽出することで、必要な情報だけを取得でき、処理をより効率的に行うことができます。

## 方法

サブストリングを抽出する方法は簡単です。まずは文字列を取得し、抽出したい部分の開始位置と長さを指定します。その後、`substr()`関数を使って、部分文字列を取得します。

```C++
// 文字列を取得
string str = "こんにちは、私はプログラマーです。";

// 抽出したい部分の開始位置と長さを指定
int start = 6; // "は"の位置から開始
int length = 6; // 6文字を抽出

// 部分文字列を取得
string sub = str.substr(start, length); // subには"は、私"が格納される

// 結果を出力
cout << sub << endl; // "は、私"が表示される
```

## 深堀り

`substr()`関数の定義は以下の通りです。

```C++
string substr (size_t pos, size_t len) const;
```

第一引数には抽出したい部分の開始位置を、第二引数には抽出する長さを指定します。第二引数は省略することができ、省略すると開始位置から文字列の末尾までの部分文字列が返されます。

また、文字列の末尾からの文字数を指定することもできます。開始位置をマイナスの値で指定した場合、末尾から数えた文字数になります。

さらに、現在の文字列の長さよりも長い文字列を指定した場合、文字列の最後までの部分文字列が返されます。しかし、開始位置が文字列の長さを超える場合はエラーが発生します。

以上のように、`substr()`関数を使いこなすことで、柔軟に部分文字列を抽出することができます。

## See Also

- [C++ string::substr](https://www.cplusplus.com/reference/string/string/substr/) - `substr()`関数の詳細な定義が記載されています。
- [C++ strings](https://www.programiz.com/cpp-programming/strings) - C++での文字列操作の基本を学べるチュートリアルです。