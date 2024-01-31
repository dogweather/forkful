---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:41:37.134795-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字のパターンにマッチするものを削除するとは、特定の条件に合う文字を文字列から取り除くことです。これによりデータの整理やユーザー入力のバリデーションなど、様々な目的で使われます。

## How to: (方法)

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    // 文字列の定義
    std::string message = "コンピューター言語123";

    // パターンにマッチする文字を削除: 数字を取り除く
    message.erase(std::remove_if(message.begin(), message.end(), ::isdigit), message.end());

    // 結果の出力
    std::cout << message << std::endl; // 出力: コンピューター言語
    return 0;
}
```

## Deep Dive (深い潜入)

最初はメモリーや容量が限られていたため、不要な文字を削除することで効率を上げていました。現在では、C++では `<algorithm>` ヘッダの `std::remove_if` や `std::erase` を使ったり、`std::regex_replace` で正規表現を使用してパターンマッチングする方法が一般的です。これらは、それぞれ削除したい文字に最適なアプローチを提供します。

## See Also (関連項目)

- C++ Reference: https://en.cppreference.com/w/
- Regular Expressions in C++: https://www.cplusplus.com/reference/regex/
- Algorithm Library: https://en.cppreference.com/w/cpp/header/algorithm
