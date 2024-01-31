---
title:                "文字列の補間"
date:                  2024-01-20T17:50:33.636795-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間とは、文字列を生成する際に変数や式の値を埋め込むことです。プログラマーはこれを使って、動的に内容が変わる文字列を容易に作成し、コードの可読性を向上させます。

## How to (やり方)
C言語の文字列補間は`printf`関数や`sprintf`関数を使って実施されます。以下に簡単な例を示します。

```C
#include <stdio.h>

int main() {
    int age = 25;
    const char *name = "Taro";

    // printfを使った例
    printf("こんにちは、私の名前は%sです。年齢は%d歳です。\n", name, age);

    // sprintfを使って文字列を生成し、後で使用する例
    char greeting[50];
    sprintf(greeting, "こんにちは、私の名前は%sです。", name);
    printf("%s年齢は%d歳です。\n", greeting, age);

    return 0;
}
```

出力:

```
こんにちは、私の名前はTaroです。年齢は25歳です。
こんにちは、私の名前はTaroです。年齢は25歳です。
```

## Deep Dive (深掘り)
文字列補間はもともとは書式指定文字列を解釈し、組み込みの変換指定子（例: `%s`、`%d`）を使って変数の値を埋め込む機能から発展しました。C言語以前からあるコンセプトですが、言語によって実装は異なります。

代替手段として、連結することで同じ結果を出すことが可能ですが、これは冗長でエラーの原因になることがあります。一方、C言語では文字列補間が`printf`や`sprintf`の形で言語の一部になっており、`%s`が文字列、`%d`が整数、`%f`が浮動小数点数として埋め込まれます。

実装の詳細については、`printf`関数は出力先として標準出力を使用し、`sprintf`関数は文字列バッファに書き出します。セキュリティを考慮して、バッファオーバーフローを防ぐ目的で`snprintf`がよく使用されます。

## See Also (関連リンク)
- [C Standard Library - printf](https://en.cppreference.com/w/c/io/fprintf)
- [C Standard Library - sprintf](https://en.cppreference.com/w/c/io/fprintf)
- [C Secure Coding Guidelines](https://wiki.sei.cmu.edu/confluence/display/c/SEI+CERT+C+Coding+Standard)
