---
title:                "C: コマンドライン引数の読み込み"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##なぜ？
コマンドライン引数を読み込むために、なぜ読者がこの記事を読むべきかを説明します。

##使い方
コードブロック（```C ... ```）内のコーディング例とサンプル出力を使用して、コマンドライン引数を読み込む方法を説明します。

```C
#include <stdio.h>
int main(int argc, char *argv[]) {
    //コマンドライン引数の数を出力
    printf("コマンドライン引数の数は%d\n", argc);
    
    //すべての引数をループして出力
    for(int i = 0; i < argc; i++) {
        printf("引数 %d: %s\n", i, argv[i]);
    }
    
    return 0;
}
```

サンプル出力：

```
コマンドライン引数の数は3
引数 0: ./a.out
引数 1: hello
引数 2: world
```

##深堀り
コマンドライン引数を読み込む際の詳細な情報を提供します。argv配列の要素のデータ型はcharポインタであり、プログラム実行時にシステムから渡された引数が格納されます。また、argcは引数の数を表す整数型の変数です。

コマンドライン引数は、プログラム起動時にコマンドラインから直接値を渡すことができるため、プログラムの柔軟性を高めることができます。例えば、実行時にファイル名やオプションを指定することで、プログラムの挙動を変更することができます。

また、コマンドライン引数を使用することで、複数のデータを一度にプログラムに渡すことができるため、処理の効率を改善することができます。

##See Also
- [Command Line Arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [コマンドライン引数（C言語）](https://programming-place.net/ppp/contents/c/cts022.html)