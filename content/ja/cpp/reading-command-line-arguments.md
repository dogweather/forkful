---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

読み込みコマンドライン引数は、コマンドラインからパラメータを取得する方法です。プログラマーはこれを使って、プログラム起動時に特定の動作を制御します。

## 使い方：

コマンドライン引数を読み取る基本的な方法を見てみましょう：

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    for(int i = 0; i < argc; i++) {
        std::cout << "Command line arg [" << i << "] is: " << argv[i] << "\n";
    }
    return 0;
}
```
このプログラムを"my-program.exe"としてコンパイルし、以下のように実行します：

`my-program.exe first-arg second-arg`

これにより、以下の出力が得られます：

``` 
Command line arg [0] is: my-program.exe
Command line arg [1] is: first-arg
Command line arg [2] is: second-arg
```

## ディープダイブ

コマンドライン引数の取得は、UNIXシェル環境とそのツールが初めて作成された時から存在します。C++はこれを効果的に組み込んで、プログラマーが実行可能ファイルをより回転配置できるようにしました。

読み取りコマンドライン引数の代替案として、環境変数を使用したり、設定ファイルを読み込ませたりすることができます。しかし、直接コマンドライン引数を使用することで、ワンライナー・コマンドや自動化スクリプトでプログラムを簡単に制御できるため、より便利です。

`main`関数は`argc`と`argv`の両方の引数を取ります。`argc`は引数の総数を示し、`argv`は引数の配列を示します。最初の引数(argv[0])は常にプログラム名です。

## 参照情報

より深く理解するために次の情報源も見てみてください：

- C++ コマンドライン引数の詳細な説明: https://www.learncpp.com/cpp-tutorial/command-line-arguments/
- 環境変数との比較: https://stackoverflow.com/questions/5457632/environment-variables-vs-command-line-arguments
- コマンドライン引数のベストプラクティス：https://www.joelonsoftware.com/2007/12/14/programmer-interview-transcript-steve-yegge/