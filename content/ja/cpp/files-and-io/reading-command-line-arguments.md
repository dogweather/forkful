---
title:                "コマンドライン引数の読み取り"
aliases:
- /ja/cpp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:45.713037-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
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
