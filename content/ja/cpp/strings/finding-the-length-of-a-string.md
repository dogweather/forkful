---
date: 2024-01-20 17:47:19.655175-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.538425-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
