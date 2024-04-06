---
date: 2024-01-20 17:50:17.202619-07:00
description: "How to / \u65B9\u6CD5 Sample output."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.042839-06:00'
model: gpt-4-1106-preview
summary: "How to / \u65B9\u6CD5 Sample output."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
