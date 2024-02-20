---
date: 2024-01-20 17:50:17.202619-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u5185\
  \u306B\u5909\u6570\u3084\u5F0F\u306E\u7D50\u679C\u3092\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u51FA\u529B\u3092\u52D5\
  \u7684\u306B\u5909\u66F4\u3057\u305F\u308A\u3001\u3088\u308A\u8AAD\u307F\u3084\u3059\
  \u3044\u30B3\u30FC\u30C9\u3092\u66F8\u304F\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.644969
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u5185\
  \u306B\u5909\u6570\u3084\u5F0F\u306E\u7D50\u679C\u3092\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u51FA\u529B\u3092\u52D5\
  \u7684\u306B\u5909\u66F4\u3057\u305F\u308A\u3001\u3088\u308A\u8AAD\u307F\u3084\u3059\
  \u3044\u30B3\u30FC\u30C9\u3092\u66F8\u304F\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
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
