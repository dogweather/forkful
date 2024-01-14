---
title:                "C++: 正規表現の使用"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使う理由は、文字列の検索や操作を簡単に行うことができるからです。

## 使い方

正規表現を使うには、まずは正しい文法を学ぶ必要があります。以下のコードを参考にしてください。

```C++
// 文字列を宣言
std::string str = "Hello, world!";

// 正規表現パターンを宣言
std::regex pattern = std::regex("Hello");

// 文字列をパターンにマッチング
bool match = std::regex_match(str, pattern);

// マッチングの結果を出力
std::cout << match; // 出力結果: true
```

また、正規表現を使うと文字列の置換や切り出しも簡単に行うことができます。以下のコードを参考にしてください。

```C++
// 文字列を宣言
std::string str = "John Doe";

// 正規表現パターンを宣言
std::regex pattern = std::regex("Doe");

// 文字列を置換
std::string result = std::regex_replace(str, pattern, "Smith");

// 置換結果を出力
std::cout << result; // 出力結果: John Smith
```

正規表現を使うことで、複雑な文字列操作が簡単に行えます。

## 深堀り

正規表現にはさまざまな機能があり、パターンマッチングやグループ化などの機能についても詳しく学ぶことができます。また、文字列以外のデータ型にも正規表現を使うことができます。さらに、正規表現の性能を最大限に引き出すための最適化方法も存在します。

正規表現は初心者にとっては少し難しいかもしれませんが、慣れてしまえばとても便利なツールです。是非活用してみてください。

## 関連情報

- [C++正規表現チュートリアル](https://cpprefjp.github.io/reference/regex/basic_regex.html)
- [正規表現が使えるC++ライブラリ「Boost Regex」](https://qiita.com/THE_BORE/items/ee1f886b110da22fe012)
- [C++で正規表現を最適化する方法](https://www.slideshare.net/ryotako/cpp-84370031)