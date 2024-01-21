---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:45:18.442816-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
文字列から部分文字列を取り出すことは、指定した範囲の文字列を抜き出す行為です。これにより特定の情報を処理しやすくしたり、データを解析しやすくなります。

## How to (方法)
```C++
#include <iostream>
#include <string>

int main() {
    std::string sentence = "こんにちは、C++の世界へようこそ！";
    std::string greeting = sentence.substr(0, 5); // はじめの5文字を抜き出す
    
    std::cout << greeting << std::endl; // 出力: こんにちは

    std::string welcome = sentence.substr(6); // 6文字目から最後まで抜き出す
    
    std::cout << welcome << std::endl; // 出力: C++の世界へようこそ！
}
```

## Deep Dive (深掘り)
C++での部分文字列抽出は、`std::string`クラスの`substr`関数で実現します。これは初版のC++標準ライブラリに含まれていました。代替手段として`std::string_view`やC言語の`strncpy`関数も考えられますが、利便性や安全性で`substr`が優れています。`substr`は、指定した開始位置と長さに基づいて新しい文字列を生成しますが、メモリのコピーが関与するため、大きな文字列での操作には注意が必要です。

## See Also (関連情報)
- C++ Reference `std::string::substr`: https://en.cppreference.com/w/cpp/string/basic_string/substr
- C++ Reference `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- CPP `std::string` Guide: https://www.cplusplus.com/reference/string/string/