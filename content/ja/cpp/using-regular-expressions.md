---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
正規表現って何？テキストデータからパターンを探すための強力な方法だ。プログラマはなぜ使う？効率的に文字列を検索、置換、解析するため。

## How to: (方法)
`<regex>`ヘッダで正規表現使う。サンプルコードをチェック。

```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string data = "これはテストです123";
    std::regex pattern(R"(\d+)"); // 数字の一致を探すパターン

    // 探索と表示
    std::smatch matches;
    if (std::regex_search(data, matches, pattern)) {
        std::cout << "見つかった数字: " << matches[0] << std::endl;
    } else {
        std::cout << "数字が見つかりませんでした。" << std::endl;
    }

    return 0;
}
```
出力:
```
見つかった数字: 123
```

## Deep Dive (詳細な情報)
正規表現は、1970年代にUNIX環境で使われ始めた。標準の`<regex>`ライブラリ以外に、Boost.Regexのようなライブラリもある。C++の実装では、NFA(非決定性有限オートマトン)とDFA(決定性有限オートマトン)を使って動作する。

## See Also (参考情報)
- C++ `<regex>` Reference: [cppreference.com/w/cpp/regex](https://en.cppreference.com/w/cpp/regex)
- Regex Tester: [regex101.com](https://regex101.com)
- Boost.Regex Documentation: [boost.org/doc/libs/release/libs/regex/](https://www.boost.org/doc/libs/release/libs/regex/)