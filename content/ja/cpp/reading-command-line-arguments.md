---
title:                "コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み込み"
html_title:           "C++: コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み込み"
simple_title:         "コンピュータープログラミングの記事のタイトル：コマンドライン引数の読み込み"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何か&なぜ?

コマンドライン引数を読み取るとは、プログラマーがプログラムに与えられたコマンドラインから情報を受け取ることです。コマンドライン引数は、プログラムの動作を変更したり、外部から入力を受け取ったりするために使用されます。

## 方法:

```C++
#include <iostream>

using namespace std;

int main(int argc, char** argv) {
    // コマンドライン引数の数を出力
    cout << "引数の数: " << argc << endl;
    // 引数の値を出力
    for(int i = 0; i < argc; i++){
        cout << "引数 " << i << ": " << argv[i] << endl;
    }
    return 0;
}
```
### 出力例:

```
引数の数: 4
引数 0: プログラム名
引数 1: 引数1
引数 2: 引数2
引数 3: 引数3
```

## 深堀り:

コマンドライン引数は、プログラムが実行される前に指定される必要があります。この機能はメインフレームでよく使われていた時代から存在しており、今でも多くのプログラムで使用されています。コマンドライン引数を受け取る方法としては、引数として与えられた文字列を直接取得する方法以外にも、標準ライブラリ関数を使用する方法や、環境変数を使用する方法などがあります。

## 関連情報:

- [C++ Reference - コマンドライン引数を受け取る](https://cpprefjp.github.io/reference/dynamic/standard-input-output.html) 
- [C++コマンドライン引数チュートリアル](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)