---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"

category:             "C++"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を大文字にするとは、例えば "hello world" を "HELLO WORLD" に変換することです。読みやすさを高めたり、ユーザーインターフェースでの一貫性を確保したり、特定のプログラミング要件を満たすために行います。

## How to: (方法)
```C++
#include <iostream>
#include <algorithm>
#include <string>

std::string capitalizeString(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c) -> unsigned char { return std::toupper(c); });
    return s;
}

int main() {
    std::string original = "konbanwa world";
    std::string capitalized = capitalizeString(original);
    std::cout << "Original: " << original << "\n";
    std::cout << "Capitalized: " << capitalized << std::endl;
    return 0;
}
```

実行すると、この出力が得られます:
```
Original: konbanwa world
Capitalized: KONBANWA WORLD
```

## Deep Dive (詳細な解説)
文字列を大文字にするという行為は、元はタイプライターや印刷技術から始まりました。大文字は重要な単語や文を際立たせるために使われてきました。プログラミングにおいても、ユーザーがすべて大文字のテキストを目にしやすくするため、またはデータベースの一貫性を保つために使われます。

C++では、`<algorithm>` ヘッダの `std::transform` 関数と`<cctype>` ヘッダの `std::toupper` を使うのが一般的です。他の言語でも似たような機能を持つ関数があります。

実装の詳細では、ラムダ式が使われていて、各文字を大文字に変換するために `std::toupper` を適用しています。これはエレガントであり、また効率的です。

また、これ以外の手法としては、C++の古いバージョンでのループや`<locale>` ヘッダでのロケールに依存した方法などがあります。

## See Also (関連情報)
- C++ reference for `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ reference for `std::toupper`: https://en.cppreference.com/w/cpp/string/byte/toupper
- Locale-specific capitalization with `std::toupper`: https://en.cppreference.com/w/cpp/locale/toupper
