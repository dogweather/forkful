---
date: 2024-01-20 17:47:19.655175-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3053\
  \u3068\u306F\u3001\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\u306E\
  \u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30E1\u30E2\u30EA\u7BA1\u7406\u3001\u5165\u529B\u691C\u8A3C\
  \u3001\u307E\u305F\u306F\u30EB\u30FC\u30D7\u306E\u5236\u9650\u3068\u3044\u3063\u305F\
  \u7406\u7531\u304B\u3089\u3001\u9577\u3055\u3092\u77E5\u308B\u5FC5\u8981\u304C\u3042\
  \u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.498382-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3053\
  \u3068\u306F\u3001\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\u306E\
  \u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30E1\u30E2\u30EA\u7BA1\u7406\u3001\u5165\u529B\u691C\u8A3C\
  \u3001\u307E\u305F\u306F\u30EB\u30FC\u30D7\u306E\u5236\u9650\u3068\u3044\u3063\u305F\
  \u7406\u7531\u304B\u3089\u3001\u9577\u3055\u3092\u77E5\u308B\u5FC5\u8981\u304C\u3042\
  \u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを見つけることは、文字列に含まれる文字の数を数えることです。プログラマーは、メモリ管理、入力検証、またはループの制限といった理由から、長さを知る必要があります。

## How to: (方法)
```C++
#include <iostream>
#include <string>

int main() {
    std::string myString = "こんにちは世界";
    std::cout << "The length of the string is: " << myString.length() << std::endl;
    // C++11以降、以下のようにも書けます
    std::cout << "The length of the string is: " << myString.size() << std::endl;
    return 0;
}
```
出力:
```
The length of the string is: 21
The length of the string is: 21
```

## Deep Dive (詳細な解説)
文字列の長さを知る機能は、C++の初期から存在します。C言語では`strlen`関数が使われていましたが、C++では`std::string`クラスで`.length()`や`.size()`メソッドが提供されています。これらのメソッドは、同じ結果を返しますが、適用する文脈によって使い分けることが推奨されます（`.length()`は文字列の長さが、`.size()`はコンテナの大きさが焦点の時に使います）。

内部的には、`std::string`は文字の配列を管理しつつ、長さの情報を追跡することで高速に長さを報告します。これは計算の必要がなく、効率的です。

他にも、C++スタイルでは`std::string`を使いますが、Cスタイルの文字列（NULLで終了する文字配列）でも長さは`strlen`関数で取得可能です。しかし、安全性や使い勝手で`std::string`が推奨されます。

## See Also (関連情報)
- [std::string::length - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [std::string::size - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string/size)
- [C++ Strings (std::string)](https://www.cplusplus.com/reference/string/string/)
