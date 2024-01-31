---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:12:15.014127-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPL（Read-Eval-Print-Loop; 読み取り-評価-出力-ループ）は、シンプルで対話的なプログラミング環境です。プログラマーは、リアルタイムで言語を試す、簡単なタスクを行う、またはフルブローンのアプリケーションを作成するオーバーヘッドなしに新しい概念を理解するためにこれを使用します。

## 方法：
C++には組み込みのREPLが付属していませんが、Clingのようなツールでその機能を提供することができます。ここでは、Clingを使用して2つの数値の合計を計算する方法を示します：

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "The sum is: " << a + b << std::endl;
    return 0;
}

// 出力:
// 合計は：12
```

Clingを起動し、コードを1行ずつ入力して、各コマンドの後に出力を観察します。コンパイルなしで即座にフィードバックを受け取ります。

## 深掘り
REPLはPythonやLispのような言語に一般的であり、1960年代から存在しています。C++のようなコンパイル言語には概念が自然には合わないため、Clingのようなツールが存在します。これらはC++を即時に解釈します。代替手段にはオンラインコンパイラーや従来の方法でコンパイルされた小規模のテストプログラムが含まれます。ClingはLLVMとClangの上に構築されており、解釈方式でC++を使用するための橋渡しを提供します。

## 参照
- [Cling](https://root.cern/cling/): LLVMとClangライブラリの上に構築されたインタラクティブなC++インタプリタ。
- [Jupyter Notebooks](https://jupyter.org/): ノートブック環境内でインタラクティブシェルを提供し、xeus-clingカーネルを通じてC++をサポート。
- [LLVM](https://llvm.org/): コンパイラやツールチェーン技術のモジュール式で再利用可能なコレクション。Clingが基盤とするもの。
