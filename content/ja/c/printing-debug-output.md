---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:51:56.182277-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力とは、コードがどう動いているかを確認するために、メッセージや変数の値をコンソールに表示させることです。プログラマは問題を発見し、ロジックを理解しやすくするためにこれを行います。

## How to: (やり方)
C言語でデバッグ出力をする一番簡単な方法は、`printf`関数を使うことです。以下はその例です。

```C
#include <stdio.h>

int main() {
    int loop_index = 0;

    for(loop_index = 0; loop_index < 5; loop_index++) {
        printf("loop_index is now: %d\n", loop_index);
    }

    return 0;
}
```
出力サンプル:
```
loop_index is now: 0
loop_index is now: 1
loop_index is now: 2
loop_index is now: 3
loop_index is now: 4
```

## Deep Dive (深いダイビング)
デバッグ出力はC言語が生まれた1970年代から使われていますが、その方法は時間と共に進化しました。`printf`はシンプルでありながら強力なツールですが、大規模なアプリケーションではログレベルやログファイルへの出力を制御する専門のライブラリが使われることがあります。

実装の詳細では、デバッグメッセージがリリースビルドに残らないように、プリプロセッサディレクティブを使って条件付きコンパイルを行うことが一般的です。

```C
#include <stdio.h>

int main() {
    #ifdef DEBUG
    printf("Debugging is ON.\n");
    #endif

    return 0;
}
```
コンパイル時に`-DDEBUG`オプションを使うと、デバッグメッセージが表示されます。

## See Also (関連情報)
- [GNU C Library: Formatted Output Functions](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output-Functions.html)
- [C Preprocessor: Conditional Syntax](https://gcc.gnu.org/onlinedocs/cpp/Conditional-Syntax.html)
- [Debugging with GDB](https://www.gnu.org/software/gdb/documentation/)
