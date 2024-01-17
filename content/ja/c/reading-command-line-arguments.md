---
title:                "コンピュータプログラミングの記事：コマンドライン引数の読み取り"
html_title:           "C: コンピュータプログラミングの記事：コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングの記事：コマンドライン引数の読み取り"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何のために？
コマンドライン引数の読み込みとは、プログラマーがプログラムに与えられた引数を読み取ることです。プログラマーは、プログラムが実行されるときにどのような動作をするかを決定するために、引数を使います。

## 方法：
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  // プログラム名を表示
  printf("プログラム名： %s\n", argv[0]);

  // 第1引数を表示
  printf("引数1： %s\n", argv[1]);

  // 第2引数を表示
  printf("引数2： %s\n", argv[2]);

  return 0;
}
```

### 入力：
```bash
./a.out Hello World
```

### 出力：
```
プログラム名： ./a.out
引数1： Hello
引数2： World
```

## 深掘り：
コマンドライン引数の読み込みは、プログラムを動的に制御するための重要な手段です。これにより、ユーザーがプログラムの振る舞いをカスタマイズすることができます。

コマンドライン引数の代わりとなるオプションとして、環境変数を使うこともできます。しかし、環境変数はプログラムが実行される環境によって異なる可能性があり、複数のプログラム間での共有が困難な場合があります。

コマンドライン引数は、プログラムが実行される前にシェルによって解析されます。そのため、プログラム側ではコマンドライン引数の解析を行う必要がありません。

## 関連リンク：
- [C言語入門：コマンドライン引数](https://www.grapecity.com/toolkit/jp/spread/Help/sp_CCommandLine_001.html)
- [C: プログラムにコマンドライン引数を渡す](https://programming-summary.com/c-passing-command-line-arguments/)