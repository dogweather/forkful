---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りは、プログラムの起動時に渡されるオプションやデータを取得する方法です。これにより、ユーザーがプログラムの動作を動的に制御できるようになります。

## 実際の方法：

以下に簡単なコード例を示します。この例では、引数の数と引数自体を表示します。
```C
#include<stdio.h>

int main(int argc, char *argv[]) {
    int counter;

    printf("Program Name Is: %s",argv[0]);

    if(argc==1)
        printf("\nNo Extra Command Line Argument Passed Other Than Program Name");
    if(argc>=2)
    {
        printf("\nNumber Of Arguments Passed: %d",argc);
        printf("\n----Following Are The Command Line Arguments Passed----");

        for(counter=0; counter<argc; counter++)
            printf("\nargv[%d]: %s", counter, argv[counter]);
    }

    return 0;
}
```
仮に上記プログラムを `arg_reader.exe` として、次のように引数を渡して起動します: `arg_reader.exe One Two Three`

出力は次のようになります:
```
Program Name Is: arg_reader.exe
Number Of Arguments Passed: 4
----Following Are The Command Line Arguments Passed----
argv[0]: arg_reader.exe
argv[1]: One
argv[2]: Two
argv[3]: Three
```

## 掘り下げ：

コマンドライン引数の使用は、プログラミングが始まった当初から存在し、こうした引数なしには多くのユーティリティが存在できないと言えます。より進んだ方法として `getopt()` や `getopt_long()` 関数があることにも触れておきます。これらの関数を利用すると、コマンドラインオプションのパースが容易になります。

基本的に `argc` と `argv` を通じてコマンドライン引数を読み取る際、`main()` 関数が OS からこれらの情報を受け取ると考えてよいです。`argc` は引数の数を示し、`argv` の各要素が個々の引数を指しています。

## 参考資料：

1. コマンドライン引数について詳しく説明している英語の記事: https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm
2. `getopt()` 関数の使い方: https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html
3. `getopt_long()` 関数の使い方: https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Options.html