---
title:                "C++: コンピューター プログラミングの記事「コマンドライン引数を読む」"
simple_title:         "コンピューター プログラミングの記事「コマンドライン引数を読む」"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み込むことの重要性についてお話します。コマンドライン引数を使用することで、ユーザーがプログラムの実行中に入力する値を制御することができます。プログラムの動作を変えたり、特定の機能を実行したりするのに役立ちます。

## 使い方

C++でコマンドライン引数を読み込む方法を学びましょう。まずは以下のコード例をご覧ください。

```C++
#include <iostream>

int main(int argc, char* argv[]) {
  for (int i = 0; i < argc; i++) {
    std::cout << "引数" << i << ": " << argv[i] << std::endl;
  }
}
```

このコードはプログラムが受け取った全てのコマンドライン引数を出力するものです。

例えば、コマンドプロンプトで`./program 私はC++が大好きです`と入力すると、以下のような出力が得られます。

```
引数0: ./program
引数1: 私はC++が大好きです
```

ここで`argc`は引数の数を表し、`argv`は実際の引数の配列を指します。`argv[0]`は実行ファイルの名前を示し、それ以降が入力されたコマンドライン引数となります。

さらに、特定のフラグを使用してコマンドライン引数を受け取ることもできます。例えば、`./program -n 123`と入力した場合には以下のようにフラグを使用して特定の操作を行うことができます。

```C++
#include <iostream>

int main(int argc, char* argv[]) {
  int num = 0;
  for (int i = 0; i < argc; i++) {
    if (argv[i] == "-n") {
      num = std::stoi(argv[i+1]);
      break;
    }
  }

  std::cout << "入力された数値は" << num << "です" << std::endl;
}
```

このように、コマンドライン引数はプログラムをより柔軟にするための重要なツールです。

## 詳しく見ていく

コマンドライン引数を使用する際には、いくつか注意すべき点があります。まずは自分のプログラムでどのようなコマンドライン引数を使用するかを決めることが重要です。また、予期しない入力に対しては適切なエラーハンドリングを行うことも重要です。さらに、プログラムの使用方法をユーザーに理解してもらうためのヘルプメッセージを表示することも考慮すべきでしょう。

## 参考リンク

- [C++でコマンドライン引数を読み込む方法](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C++コマンドライン引数チュートリアル](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [C++の標準ライブラリでコマンドライン引数を使う方法](https://blog.feabhas.com/2020/02/the-c-standard-library-and-command-line-arguments/)