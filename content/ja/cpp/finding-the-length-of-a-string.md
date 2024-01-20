---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列の長さを求めるとは、文字列に含まれる文字の数を数えることです。プログラマは、この長さを使って、文字列を操作志向する。また、範囲を超えるエラーを防ぐためにも使用します。

## どうやって：
C++で文字列の長さを求める基本的な方法は、`std::string`の`size()`または`length()`メソッドを使用することです。

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "こんにちは、世界!";
    std::cout << "長さは: " << str.length() << std::endl;
    return 0;
}
```

このコードは次の出力を表示します:

```
長さは: 9
```

## 詳細:
文字列の長さを求めるための方法は多数存在します。上述の方法は現代的で最も直接的な方式であり、以前のC++バージョンでも使用可能です。

Cスタイルの文字列では、`strlen`関数を使用して文字列の長さを取得できます。ただし、こちらはC++専用の機能ではなく、Null終端文字列に対してのみ有効です。

最終的には、使う方法はあなたの要件と好みによるところが大きいです。ただし、安全さと簡単さから言えば、`std::string`の`length()`または`size()`メソッドを推奨します。

## 参考になるリンク:
- [std::stringのリファレンス](http://www.cplusplus.com/reference/string/string/)
- [C++ Reference - Len Function](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [C++ Reference - Size Function](https://en.cppreference.com/w/cpp/string/basic_string/size)

この記事が文字列の長さを求めるための理解に役立つことを願っています。