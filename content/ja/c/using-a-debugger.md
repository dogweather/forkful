---
title:                "デバッガーの使い方"
date:                  2024-01-26T03:47:50.688391-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガは、Cコードがステップバイステップで実行されるのを検査し、バグを追い詰めることができるツールです。プログラマーはデバッガを使用して、コードの動作を理解し、問題を修正し、推測ゲームを行うことなくパフォーマンスを最適化します。

## 使い方：
ある数の階乗を計算するシンプルなCプログラムを扱っていて、何か不具合があるとします。`gdb`（GNU Debugger）のようなデバッガを使用するには、まずデバッグ情報を含めるために`-g`フラグを使用してコンパイルします：
```c
// コンパイル方法: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // 負の入力に対する簡単なチェック
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("The factorial of %d is %ld\n", number, result);
    return 0;
}
```

次に、gdbで実行します：
```shell
$ gdb ./factorial
```

`factorial`関数でブレークポイントを設定し、プログラムを実行します：
```gdb
(gdb) break factorial
(gdb) run
```

ブレークポイントに到達すると、`next`または`n`を使用して各行をステップスルーし、`print`または`p`で変数を検査します：
```gdb
(gdb) next
(gdb) print result
$1 = 1
```

サンプルの出力は、リアルタイムの値とプログラム実行フローを提供します。

## ディープダイブ
デバッガは1960年代から存在しており、シンプルなモニターから複雑なGUIベースのアプリケーションに進化してきました。成熟したデバッガが開発される前は、古典的なプリントベースのデバッグが一般的でした。`gdb`の代わりとなるものには、`lldb`、`dbx`、またはVisual StudioやCLionのようなIDE統合デバッガがあります。

デバッガを扱う場合、実装はさまざまです—一部はランタイムエラーをキャッチしたり、メモリを調査したり、プログラムの実行を逆にしたりすることができます。`gdb`は実行中のプロセスにアタッチでき、既に実行中のソフトウェアのデバッグを可能にし、ライブシステムバグの修正に役立ちます。

## 参照
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- GDBでのデバッグ: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- C言語におけるデバッグ技術: http://www.cprogramming.com/debugging/debugging.html
