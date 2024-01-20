---
title:                "文字列を大文字にする"
html_title:           "C++: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 文字列の大文字化を行うC++のプログラミング

## 何と何故？
文字列の大文字化とは、文字列内の全ての小文字を大文字に変換することを言います。プログラマーは統一性を持たせ、比較を容易にするために文字列を大文字化します。

## 方法:
以下に示すのがC++で文字列を大文字に変換する一例です。

```C++
#include <algorithm>
#include <cctype>
#include <iostream>

int main() {
    std::string s = "Hello, World!";
    std::transform(s.begin(), s.end(), s.begin(), ::toupper);
    std::cout << s;
    return 0;
}
```
以上を実行すると、アウトプットは全て大文字の"HELLO, WORLD!"になります。

## 詳細の掘り下げ
**1.  歴史的背景:** C++には文字列操作のための標準関数とライブラリが提供されていますが、それらはC言語がベースで、'std::transform'と'toupper'の組み合わせは現代的なC++スタイルとなります。

**2.  代替案:** 'for'ループを使って各文字を大文字に変換する方法もあります。しかし'std::transform'関数は機能的なアプローチを提供し、コードの可読性を高めます。

``` C++
std::string s = "Hello, World!";
for(char &c : s) c = std::toupper(c);
std::cout << s;
```
こちらのコードでも同じ結果が得られます。

**3.  実装の詳細:** 'std::transform'関数は範囲全体に一定の操作を適用します。ここでは各文字にtoupper関数が適用され、小文字が大文字に変換されます。

## 参照リンク
以下に、関連する参考リンクをいくつか掲載します:

1. [C++ String Library - cppreference.com](https://en.cppreference.com/w/cpp/string)
2. [std::toupper - cppreference.com](https://en.cppreference.com/w/cpp/string/byte/toupper)
3. [std::transform - cppreference.com](https://en.cppreference.com/w/cpp/algorithm/transform)