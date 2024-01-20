---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列を小文字に変換するとは、特定の文字列内のすべての大文字を対応する小文字に変えることを意味します。プログラマーは、文字列の比較や検索時に、大文字と小文字の違いを無視するためにこれを行います。

## 方法:

C++のstdライブラリにはtransform関数が含まれており、これを使うことで簡単に文字列を小文字に変換することができます。

```C++
#include <algorithm>
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";
    std::transform(str.begin(), str.end(), str.begin(),
                   [](unsigned char c){ return std::tolower(c); });

    std::cout << str;
    return 0;
}
```

このコードを実行すると、出力は全て小文字の "hello, world!" になります。

## 詳細解説:

`std::transform`関数はSTL(Standard Template Library)の一部で、古いC++標準から存在しています。文字列を小文字に変換することは、ASCIIやUnicodeのような文字セットを扱うことで可能になります。具体的には、各文字に対応した小文字のコードを知ることで、変換が行われます。

代替としては、ループを使用し、各文字を個別に小文字に変換する方法もあります。しかし、transform関数を使用するとコードがシンプルで可読性が高まります。

## 関連リンク:

[transform関数の詳細](http://www.cplusplus.com/reference/algorithm/transform/)
[std::tolower関数の詳細](http://www.cplusplus.com/reference/cctype/tolower/)