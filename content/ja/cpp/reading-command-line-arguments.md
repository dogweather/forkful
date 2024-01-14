---
title:                "C++: コンピューター・プログラミングの記事のタイトル：「コマンドライン引数の読み取り」"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読む方法を学ぶことは、プログラミングにおいて非常に重要です。引数を読むことで、プログラムをより柔軟に操作することができます。

## 読み込み方
コマンドライン引数を読むには、`argc`と`argv`という２つのパラメーターを使います。以下の例を参考にしてください。

```C++
#include <iostream>

// argcは引数の数、argvは引数の値を格納する
int main(int argc, char *argv[]) {
    // 引数の数を出力
    std::cout << "引数の数: " << argc << std::endl; 

    // 引数の値をすべて出力
    for (int i = 0; i < argc; i++) {
        std::cout << "引数" << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```

**出力:**
```
引数の数: 3
引数0: プログラム名.cpp
引数1: Hello
引数2: World
```

## 深い掘り下げ
コマンドライン引数を読むことで、使用者はプログラムの実行時にパラメーターを渡すことができます。これにより、同じプログラムでも異なる結果を得ることができます。また、コマンドライン引数を利用することで、ユーザーがプログラムをよりスムーズに操作できるようになります。

## 併せて参照
- [C++でコマンドライン引数を読み込む方法](https://www.includehelp.com/cpp-programming-questions/how-to-take-command-line-arguments-in-cpp.aspx)
- [コマンドライン引数の一般的な使い方](https://www.lifewire.com/pass-arguments-to-command-line-programs-2202076)
- [C++入門: コマンドライン引数を使ってプログラムの挙動を変える](https://www.youtube.com/watch?v=mur3lxZxb1Q)