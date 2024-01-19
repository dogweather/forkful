---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何と何のため？ (What & Why?)
テキスト検索と置換は、指定された文字列を見つけ、必要に応じて他の文字列で置き描えるプロセスです。これは主に、プログラマがコードの修正やデバッグを効率的に行うために使用されます。

## 実践方法 (How to:)
C++では、`std::string`クラスの`find()`と`replace()`関数を使用してテキストの検索と置換を実行できます。具体的な実行例を見てみましょう。

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, C++ World!";
    std::size_t index = str.find("C++");

    if(index != std::string::npos) {
        str.replace(index, 3, "JAVA");
    }

    std::cout << str << std::endl;  // "Hello, JAVA World!"
    return 0;
}
```

このコードでは、`find()`関数が"C++"という文字列の位置を見つけ、`replace()`関数がそれを"JAVA"に置換しています。

## ディープダイブ (Deep Dive)
テキスト検索と置換は、プログラミング言語の早い段階から存在しており、現代の多くの高レベル言語では組み込み関数として提供されています。それらはストリング操作に不可欠なもので、一部のパフォーマンスクリティカルなアプリケーションでは、より高度な実装が必要となる場合があります。

C++には、`std::regex`や`boost::algorithm::find_regex()`などのより複雑な検索や置換のオプションも提供しています。これらは正規表現を使用してパターンをマッチさせ、より高度な置換を可能にします。

## 参考資料 (See Also)
- [C++標準ライブラリ: string](https://ja.cppreference.com/w/cpp/string/basic_string)
- [C++での正規表現処理](https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html)