---
title:                "文字列の補間"
date:                  2024-01-20T17:50:17.202619-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

category:             "C++"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
文字列補間とは、文字列内に変数や式の結果を埋め込むことです。プログラムの出力を動的に変更したり、より読みやすいコードを書くために使用されます。

## How to / 方法
```C++
#include <iostream>
#include <string>

int main() {
    int age = 25;
    std::string name = "Taro";
    
    // C++20から導入されたstd::formatを使って文字列を補間する
    std::string greeting = std::format("Hello, {}! You are {} years old.", name, age);
    
    std::cout << greeting << std::endl;
    
    return 0;
}
```

Sample output:
```
Hello, Taro! You are 25 years old.
```

## Deep Dive / 深掘り
C++には長い間、文字列補間の組み込み機能がありませんでした。従来は文字列ストリームの`std::ostringstream`や`std::to_string`を使ったり、フォーマットライブラリを利用していました。しかし、C++20で導入された`std::format`は、Pythonの`str.format()`に似た使い勝手を提供し、コードの簡潔さを改善します。

代替として、Boost.Formatライブラリや`printf`スタイルのフォーマット関数（`sprintf`など）がありますが、`std::format`は型安全で利便性が高く、現代的なC++コードに適しています。

文字列補間の実装は、渡された変数や式を文字列に変換し、指定された形式で他の文字列と結合することにより行われます。`std::format`では書式指定子に基づいており、様々なカスタマイズが可能となっています。

## See Also / 関連情報
- [`std::format` reference at cppreference.com](https://en.cppreference.com/w/cpp/utility/format/format)
- [Boost.Format library](https://www.boost.org/doc/libs/release/libs/format/)
- [Python's `str.format()` method](https://docs.python.org/3/library/stdtypes.html#str.format)
