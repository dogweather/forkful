---
title:                "テキストの検索と置換"
aliases:
- ja/cpp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:13.838156-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストの検索と置換は、ある文字列を別の文字列で置き換えることです。プログラマーはバグ修正、コード更新、データ変換などのために行います。

## How to: (方法)
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "こんにちは、プログラミングの世界へようこそ!";
    std::string from = "プログラミング";
    std::string to = "C++";

    // 検索と置換
    auto start_pos = text.find(from);
    if(start_pos != std::string::npos) {
        text.replace(start_pos, from.length(), to);
    }

    std::cout << text << std::endl;

    return 0;
}

// 出力: こんにちは、C++の世界へようこそ!
```

## Deep Dive (深掘り)
テキストの置換は歴史的にエディタの置換機能でよく用いられてきました。C++では `<string>` と `<algorithm>` ヘッダ内の関数で置換が可能です。`find()` と `replace()` は基本的なメソッドですが、正規表現を使って複雑なパターンに対応することもできます( `<regex>` を参照)。`std::regex_replace` は検索と置換を一つの手順で実行し、より強力です。代替として、Boostライブラリなどのサードパーティライブラリを使用できますが、C++11から正規表現は標準ライブラリの一部になりました。

## See Also (関連情報)
- C++ Reference std::string: [https://en.cppreference.com/w/cpp/string/basic_string](https://en.cppreference.com/w/cpp/string/basic_string)
- C++ Reference std::regex: [https://en.cppreference.com/w/cpp/regex](https://en.cppreference.com/w/cpp/regex)
- Boost String Algorithms Library: [https://www.boost.org/doc/libs/release/libs/algorithm/string/](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
