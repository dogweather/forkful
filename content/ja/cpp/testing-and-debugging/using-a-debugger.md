---
title:                "デバッガーの使い方"
aliases:
- /ja/cpp/using-a-debugger.md
date:                  2024-01-26T03:48:42.532660-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーを使用するということは、実行中のプログラムの内部を覗き見るツールを起動するということです。これにより、実際に何が起きているのかを理解することができます。プログラマーはこれを行って、コードが予期しない動作をしたり、クラッシュしたりする原因となる厄介な問題＝バグを見つけ出して潰すために行います。

## 使い方：
C++はGDBやVisual Studioデバッガーのようなデバッガーと統合しています。以下にGDBを使用した簡単な例を示します。

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // おっと、ゼロ除算です！
    std::cout << c << std::endl;
    return 0;
}

// コンパイルするには：
// g++ -g -o my_program my_program.cpp

// デバッガーで実行する：
// gdb ./my_program
```

GDBを起動したら、ブレークポイントを設定したり、コードをステップ実行したり、変数を調べたり、その他多くの操作ができます。上記を実行すれば、ゼロ除算のためにプログラムがクラッシュするのが見られるでしょう。

## 深掘り
デバッグは、プログラミングの初期段階において、文字通りハードウェアから虫（昆虫！）を取り除く必要があったことにルーツがあります。それ以来、デバッグツールは進化を遂げ、開発に不可欠な複雑で強力なソフトウェアになりました。

C++用のGDB以外の選択肢には、LLDB、Visual Studio、CLion、EclipseなどのIDEに統合されたデバッガーがあります。これらの現代的な環境は、グラフィカルなインターフェースを提供し、デバッグをより親しみやすくします。

デバッガーの使用に関する実装の詳細は、開発環境によって異なることがよくあります：

- コマンドラインデバッガー（GDB、LLDB）では、ターミナルコマンドに慣れていることが必要であり、しばしば学習曲線が急です。
- グラフィカルデバッガーは、ブレークポイントを設定したり、コードをステップ実行したり、変数を監視したりするためのポイントアンドクリックの相互作用により、プロセスを簡単にします。

条件付きブレークポイント、ウォッチポイント、式の評価など、デバッガーの能力を理解することは、問題の診断における効率を大幅に向上させることができます。

## 参照してください
- [GDBドキュメント](https://www.gnu.org/software/gdb/documentation/)
- [LLDBコマンドドキュメント](https://lldb.llvm.org/use/map.html)
- [Visual Studioデバッガーチュートリアル](https://docs.microsoft.com/ja-jp/visualstudio/debugger/debugger-feature-tour)
- [CLionでのデバッグ](https://www.jetbrains.com/help/clion/debugging-code.html)
