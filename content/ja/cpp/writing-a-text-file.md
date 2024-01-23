---
title:                "テキストファイルの書き込み"
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルの書き込みとは、文字情報をファイルに保存すること。データ永続性、設定ファイル、ログ出力等のために行われる。

## How to: (やり方)
次のコードは、`std::ofstream` を使って "example.txt" に "Hello, C++!" を書き込みます。

```C++
#include <fstream>
#include <iostream>

int main() {
    std::ofstream outfile("example.txt");

    if (outfile.is_open()) {
        outfile << "Hello, C++!" << std::endl;
        outfile.close();
    } else {
        std::cerr << "ファイルを開けませんでした。" << std::endl;
    }

    return 0;
}
```

`example.txt` の出力:

```
Hello, C++!
```

## Deep Dive (深堀り)
最初に、テキストファイル書き込みの基本。`std::ofstream` は、C++でのファイル操作に使われる標準ライブラリの一部です。C言語時代からあった `FILE*` と `fprintf` よりも、タイプセーフかつ例外を扱えるのが特徴。`std::ofstream` は RAII 原則に従い、ファイルリソースを初期化と解放を自動で行います。これ以外にも、`std::filesystem` でのファイル書き込みなど、他の方法も存在しますが、通常は `std::ofstream` が使われます。

## See Also (関連情報)
- [cppreference.com の std::ofstream](https://en.cppreference.com/w/cpp/io/basic_ofstream)
- [C++ File I/O in C++ Standard Library](https://www.cplusplus.com/doc/tutorial/files/)
- [std::filesystem を使ったファイル処理ガイド](https://en.cppreference.com/w/cpp/filesystem)
