---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？(What & Why?)
文字列から部分文字列を抽出するとは、大きな文字列から小さな一部を取り出す行為です。これは、テキストデータをマニピュレートしたり、特定の情報を取得するために開発者が行います。

## 実装方法 (How to)
部分文字列の抽出をデモするためのシンプルなC++コード例です：

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";
    std::string subStr = str.substr(0, 5);

    std::cout << subStr << std::endl;

    return 0;
}
```

上記のコードを実行すると、出力結果は「Hello」となります。`substr`関数は最初の引数として開始インデックスを、二番目の引数として取り出す文字数を取ります。

## ディープダイブ (Deep Dive)
部分文字列の抽出はプログラミングで非常に一般的な操作であり、C++の初期バージョンから存在しています。`substr`関数の代わりに、+演算子、イテレータ、あるいは`std::copy`関数などを使った手法もありますが、`substr`関数が最も手軽で読みやすいので一般的に用いられます。ただし、範囲指定が無効な場合、`std::out_of_range`という例外が発生することを忘れないでください。

## 参照リンク (See Also)
* [std::stringの公式ドキュメンテーション](http://www.cplusplus.com/reference/string/string/)
* [C++の例外の詳細について](https://www.tutorialspoint.com/cplusplus/cpp_exceptions_handling.htm)