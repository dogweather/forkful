---
title:                "C: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることは、Cプログラミングで重要なスキルです。プログラムを実行する際、ユーザーが直接引数を与えることができます。そのため、プログラムをより動的に実行することができ、パラメーターを変更する必要がある場合にも便利です。

## 方法

コマンドライン引数を読み取る方法は簡単です。まず、`main()`関数の引数に`argc`と`argv`を指定します。`argc`は引数の数を示し、`argv`は引数の実際の値を格納する配列です。

下記は、引数を1つ受け取り、その値を出力するプログラムの例です。

```C 
#include <stdio.h>

int main(int argc, char *argv[]) 
{
    // 引数の数が2でない場合、エラーを出力
    if (argc != 2) {
        printf("引数が無効です。引数を1つだけ入力してください。\n");
        return 1;
    }
    // 2番目の引数を出力
    printf("引数は %s です。\n", argv[1]);
    return 0;
}
```

実行結果は以下のようになります。

```
$ ./a.out hello
引数は hello です。
```

また、複数の引数を受け取り、それらを順番に出力するプログラムの例も紹介します。

```C
#include <stdio.h>

int main(int argc, char *argv[]) 
{
    // 引数の数が1以下の場合、エラーを出力
    if (argc <= 1) {
        printf("引数が無効です。引数を2つ以上入力してください。\n");
        return 1;
    }
    // 引数を順番に出力
    for (int i = 1; i < argc; i++) {
        printf("引数 %d は %s です。\n", i, argv[i]);
    }
    return 0;
}
```

実行結果は以下のようになります。

```
$ ./a.out apple banana cherry
引数 1 は apple です。
引数 2 は banana です。
引数 3 は cherry です。
```

## 詳しく見る

コマンドライン引数を読み取るプログラムを更にカスタマイズすることができます。例えば、文字列ではなく整数や浮動小数点数を引数として受け取ることもできます。また、引数の数量や順序をチェックして、特定の引数が与えられた場合に特定の処理を行うようなプログラムも作成できます。さらに、コマンドライン引数を使用してファイルを読み取り、プログラムにデータを与えることも可能です。

そして、コマンドライン引数を使用することで、プログラムをより柔軟に、そして効率的に作成することができます。

## 関連記事

- [C言語マニュアル：Command line arguments (コマンドライン引数)](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Argc and argv in C (Cでのargcとargv)](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [How to handle command line arguments in C (Cでのコマンドライン引数の扱い方)](https://stackoverflow.com/questions/3024197/how-to-handle-command-line-arguments-in-c)