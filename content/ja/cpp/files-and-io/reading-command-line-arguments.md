---
aliases:
- /ja/cpp/reading-command-line-arguments/
date: 2024-01-20 17:55:45.713037-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\u8FFD\u52A0\
  \u60C5\u5831\u3092\u6307\u5B9A\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u30E6\u30FC\u30B6\u304C\u30D5\u30EC\u30AD\u30B7\u30D6\u30EB\
  \u306B\u632F\u308B\u821E\u3044\u3092\u5909\u66F4\u3067\u304D\u3001\u4E00\u5EA6\u306B\
  \u69D8\u3005\u306A\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3059\u308B\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3092\u4F5C\u6210\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.204891
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\u8FFD\u52A0\
  \u60C5\u5831\u3092\u6307\u5B9A\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u30E6\u30FC\u30B6\u304C\u30D5\u30EC\u30AD\u30B7\u30D6\u30EB\
  \u306B\u632F\u308B\u821E\u3044\u3092\u5909\u66F4\u3067\u304D\u3001\u4E00\u5EA6\u306B\
  \u69D8\u3005\u306A\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3059\u308B\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u3092\u4F5C\u6210\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むとは、プログラム実行時に追加情報を指定する方法です。これにより、ユーザがフレキシブルに振る舞いを変更でき、一度に様々なタスクを実行するプログラムを作成可能にします。

## How to: (方法)
```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "プログラム名: " << argv[0] << std::endl;
    
    for (int i = 1; i < argc; ++i) {
        std::cout << "引数" << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```
実行例:
```
$ ./myprogram こんにちは 世界
プログラム名: ./myprogram
引数1: こんにちは
引数2: 世界
```

## Deep Dive (より深く)
最初のC言語バージョンから`main`関数は引数を受け取れるようになっていました。`argc`は引数の数を、`argv`は引数の配列を指す。このシンプルな仕組みは、コマンドラインからの入力を柔軟に扱える。代替として`getopt`関数やライブラリーを使ったり、環境変数を読んだりする方法もあるが、基本的なアプローチはこの二つのパラメータによるものです。

## See Also (関連情報)
- C++ reference on command line arguments: https://en.cppreference.com/w/cpp/language/main_function
- getopt manual page for argument parsing: https://man7.org/linux/man-pages/man3/getopt.3.html
- Boost.Program_options for sophisticated argument parsing: https://www.boost.org/doc/libs/release/doc/html/program_options.html
