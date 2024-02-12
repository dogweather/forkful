---
title:                "文字列の連結"
aliases:
- ja/cpp/concatenating-strings.md
date:                  2024-01-20T17:34:35.464211-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (なぜか？)
文字列の連結とは、複数のテキスト片を一つに繋げることです。プログラマーはデータ表示、メッセージの組み立て、ファイルのパスなど、多様な理由からこれを行います。

## How to: (方法)
```cpp
#include <iostream>
#include <string>

int main() {
    std::string greeting = "こんにちは、";
    std::string name = "世界!";
    std::string message = greeting + name; // 文字列を連結

    std::cout << message << std::endl; // 結果を表示
    return 0;
}
```
実行結果: `こんにちは、世界!`

## Deep Dive (深い潜入)
文字列の連結はC++の初期バージョンから存在します。`std::string`クラスの`+`演算子を使うのが一般的で、これにより複数の`std::string`オブジェクトが連結できます。古いスタイルでは、Cスタイルの文字列で`strcat()`関数を使用する方法もありましたが、バッファオーバーフローの危険があります。`std::stringstream`やフォーマットされた出力も使えます。パフォーマンスを考えると、文字列の連結はコストが高い操作であることも覚えておく必要があります。

## See Also (参照)
- C++ Standard Library: https://cplusplus.com/reference/string/string/
- `std::stringstream`の使用: https://cplusplus.com/reference/sstream/stringstream/
- モダンC++のフォーマットライブラリ(fmtlib): https://fmt.dev/latest/index.html
