---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:38:05.459150-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
文字列を小文字に変換するとは、プログラム内で全てのアルファベットを小文字にする操作のことです。一貫性を保つ、比較を簡単に行う、またはユーザー入力を標準化するために、プログラマはこれを行います。

## How to: / 方法
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "こんにちは、C++ World!";
    std::transform(text.begin(), text.end(), text.begin(), 
        [](unsigned char c){ return std::tolower(c); });

    std::cout << text << std::endl;
    return 0;
}

// 出力: こんにちは、c++ world!
```

## Deep Dive / 深掘り
歴史的にC++では`<cctype>`ヘッダにある`std::tolower`関数を使用して文字を小文字に変換してきました。このアプローチはC言語由来です。C++11から`std::transform`を使う方法が推奨されます。これは範囲ベースの操作を容易にし、ラムダ式によるカスタマイズも可能にします。代替手段として、自作のループで変換を行うこともできますが、`std::transform`と`std::tolower`を組み合わせる方法が最も手軽です。ロケールに基づく変換のため`std::tolower`はロケールに敏感なオーバーロードも提供していますが、通常はデフォルトの「C」ロケールが利用されます。

## See Also / 関連情報
- C++ 言語参照: https://ja.cppreference.com/w/cpp/string/byte/tolower
- C++ `<algorithm>` ヘッダ: https://ja.cppreference.com/w/cpp/header/algorithm
- C++ Lambda 式: https://ja.cppreference.com/w/cpp/language/lambda