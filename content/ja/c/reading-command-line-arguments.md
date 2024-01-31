---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:55:24.796844-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

コマンドライン引数を読むことは、プログラムに外部から追加の情報を与えて動作を変える方法です。プログラマはこの機能を使って、柔軟性とユーザーのコントロールを高めます。

## How to: (やり方)

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Program Name: %s\n", argv[0]);
    if(argc > 1) {
        for(int i = 1; i < argc; i++) {
            printf("Argument %d: %s\n", i, argv[i]);
        }
    } else {
        printf("No additional arguments were provided.\n");
    }

    return 0;
}
```

実行例:

```
$ ./your_program foo bar baz
Program Name: ./your_program
Argument 1: foo
Argument 2: bar
Argument 3: baz
```

## Deep Dive (詳細な解説)

コマンドライン引数はUNIX時代から存在します。`main`関数は`argc`（引数の数）、`argv`（引数の値の配列）を受け取ります。`argv[0]`はプログラム名。`argc`は常に1以上です。他の方法として、ライブラリやフレームワークが提供する引数解析のための機能もあります。例えば、`getopt`関数や`argp`ライブラリーなど。これらはコマンドライン引数を扱う際に追加の便利機能を提供します。

## See Also (関連情報)

- C Standard Library Reference: https://en.cppreference.com/w/c
- GNU C Library (glibc): https://www.gnu.org/software/libc/manual/
- C FAQs on Command Line Arguments: https://c-faq.com/aryptr/index.html
