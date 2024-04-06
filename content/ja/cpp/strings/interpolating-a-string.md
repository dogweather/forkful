---
date: 2024-01-20 17:50:17.202619-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.422446-06:00'
model: gpt-4-1106-preview
summary: "\u4EE3\u66FF\u3068\u3057\u3066\u3001Boost.Format\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3084`printf`\u30B9\u30BF\u30A4\u30EB\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u95A2\u6570\uFF08`sprintf`\u306A\u3069\uFF09\u304C\u3042\u308A\u307E\u3059\
  \u304C\u3001`std::format`\u306F\u578B\u5B89\u5168\u3067\u5229\u4FBF\u6027\u304C\u9AD8\
  \u304F\u3001\u73FE\u4EE3\u7684\u306AC++\u30B3\u30FC\u30C9\u306B\u9069\u3057\u3066\
  \u3044\u307E\u3059\u3002"
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
