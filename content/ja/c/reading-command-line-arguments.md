---
title:                "コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み取り。"
html_title:           "C: コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み取り。"
simple_title:         "コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み取り。"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ

コマンドライン引数を読み取るのに役立つ方法を学びたいと思っているかもしれません。コマンドライン引数はプログラムに動的な動作を追加するための便利な方法です。

## 方法

プログラムがコマンドライン引数を受け取る方法を見てみましょう。まず、`int main(int argc, char *argv[])`という関数定義を持つメイン関数を作成します。この定義はC言語で標準的な方法です。`argc`はコマンドライン引数の数を示し、`argv`はそれらの引数の値を保持する配列です。

```
#include <stdio.h>

int main(int argc, char *argv[]) {
    // プログラム実行時に引数が与えられなかった場合
    if (argc == 1) {
        printf("コマンドライン引数を追加して再度実行してください。\n");
        return 1;
    }

    // 引数の表示
    printf("与えられた引数の数は %d です。\n", argc - 1);
    printf("引数の値は次の通りです。\n");

    for (int i = 1; i < argc; i++) {
        printf("%d: %s\n", i, argv[i]);
    }
    
    return 0;
}
```

上記のコードを`arguments.c`という名前で保存し、次のようにコマンドラインからコンパイルして実行します。

```bash
gcc arguments.c -o arguments
./arguments 引数1 引数2 引数3
```

これで、プログラムが与えられた引数の数と値を表示するはずです。例えば、引数1の値が"hello"、引数2の値が"world"、引数3の値が"こんにちは"の場合、次のように表示されます。

```
与えられた引数の数は 3 です。
引数の値は次の通りです。
1: hello
2: world
3: こんにちは
```

## 深堀り

コマンドライン引数はプログラムの動作を制御するだけでなく、柔軟性も提供してくれます。例えば、引数でファイル名を指定することで、同じプログラムを異なるファイルに対して実行することができます。

また、コマンドライン引数を受け取る際には、エスケープ文字や特殊文字に対しても適切に処理する必要があります。これにより、ユーザーが意図しない動作を引き起こすことを防ぐことができます。

# 参考

- [C言語のコマンドライン引数の受け取り方](https://www.yukun.info/blog/2012/08/how-to-use-command-line-arguments-in-c.html)
- [C言語でコマンドライン引数を受け取る](https://www.sejuku.net/blog/4755)