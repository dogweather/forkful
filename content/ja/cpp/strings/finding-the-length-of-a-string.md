---
title:                "文字列の長さを求める"
aliases:
- /ja/cpp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:19.655175-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/finding-the-length-of-a-string.md"
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
