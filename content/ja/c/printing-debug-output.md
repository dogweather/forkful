---
title:    "C: デバッグ出力のプリント"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングでデバッグ用の出力を表示することの重要性は、特定のコードの問題を特定することができるためです。出力を使用することで、コード内で何が起こっているかを理解し、バグを見つけ、修正することができます。

## 方法

以下の例では、C言語でデバッグ用の出力を表示する方法を示します。

```C
#include <stdio.h>

int main()
{
    // 単純な出力
    printf("こんにちは、世界！\n");

    // 変数の値を出力
    int a = 5;
    printf("aの値は %d です。\n", a);

    // 条件式を出力
    int b = 10;
    printf("bの値は %d です。\n", b);
    if (a < b) 
    {
        printf("aはbより小さいです。\n");
    } 
    else 
    {
        printf("aはbより大きいです。\n");
    }
    return 0;
}
```

出力例：

```
こんにちは、世界！
aの値は 5 です。
bの値は 10 です。
aはbより小さいです。
```

## ディープダイブ

デバッグ用の出力には、標準出力の他にも様々な方法があります。例えば、`fprintf()`関数を使用することで、ファイルに出力を保存することができます。また、デバッグのためには特定の情報を表示することが重要ですので、`-D`オプションを使用して、プログラムのコンパイル時にデバッグ用のコードを追加することもできます。

## See Also

- [C言語: はじめてのデバッグ方法](https://qiita.com/torimoto-shinya/items/e4aef85eacc0895c6g49)
- [デバッグ用の出力を活用してCプログラムを改善する方法](https://www.howtogeek.com/507107/how-to-print-debug-messages-in-c-programs/)
- [C言語でのデバッグ方法](https://docs.microsoft.com/ja-jp/cpp/c-language/c-debugging-and-preprocessor-directives?view=msvc-160)