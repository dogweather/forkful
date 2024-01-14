---
title:                "C++: パターンに一致する文字を削除する"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ：パターンに一致する文字を削除するのか
どのような状況で、パターンに一致する文字を削除することが必要になるのでしょうか？例えば、文章内の特定の単語や文字列を除外するために使用することができます。または、無駄なスペースや記号を削除することで、文章の整形や処理を効率的に行うことができます。

## 使い方
削除処理を行うためには、C++の標準ライブラリである`std::string`クラスとそのメソッドを使用します。以下のようなコードで、パターンに一致する文字を削除することができます。

```C++
#include <iostream> 
#include <string> 

int main() { 
    // 文章を入力します 
    std::string text = "こんにちは、世界！Hello, world!こんにちは"; 
  
    // 削除するパターンを指定します 
    std::string pattern = "こんにちは"; 
  
    // 文字列からパターンに一致する文字を削除します 
    text.erase(text.find(pattern), pattern.length()); 
  
    // 出力を表示します 
    std::cout << text << std::endl; 
  
    return 0; 
} 
```

上記のコードを実行すると、`Hello, world!こんにちは`という出力が得られます。パターンに一致する文字が削除されたことが確認できます。

## さらに深く
上記のコードでは、`std::string`クラスの`erase()`メソッドを使用していますが、実際にはパターンに一致する文字を検索するために、正規表現というものを使用することもできます。正規表現を使用することでより複雑なパターンにも対応することができます。

また、`std::string`クラスの他にも、パターンマッチングを行うためのライブラリやツールがあります。例えばBoostライブラリの`regex`モジュールや、grepコマンドなどが挙げられます。必要に応じて、適した方法を選択することが大切です。

# 参考リンク
- [C++ reference: std::string::erase](https://en.cppreference.com/w/cpp/string/basic_string/erase)
- [C++ reference: Regular expressions](https://en.cppreference.com/w/cpp/regex)
- [Boost Regex](https://www.boost.org/doc/libs/1_77_0/libs/regex/doc/html/index.html)
- [grepコマンドの使い方](https://techacademy.jp/magazine/26806) 

# 参考にしたファイル
- [C++ reference: std::string::erase](https://en.cppreference.com/w/cpp/string/basic_string/erase)
- [C++ reference: Regular expressions](https://en.cppreference.com/w/cpp/regex)