---
title:                "文字列を小文字に変換する"
aliases:
- /ja/c/converting-a-string-to-lower-case.md
date:                  2024-02-03T17:54:46.569303-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を小文字に変換する"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## なぜ & どうやって?

C言語で文字列を小文字に変換することは、与えられた文字列のすべての大文字をそれに対応する小文字に変換する作業を含みます。プログラマーはこの操作を、比較、検索操作のためのテキスト入力の標準化、または単に出力の見た目の一貫性のためによく行います。

## 方法:

Cには、高レベル言語のような文字列を直接小文字に変換する組み込みの関数はありません。しかし、C標準ライブラリ関数を使用して、このプロセスを簡単に実装することができます。以下に、文字列を小文字に変換する方法とその例をステップバイステップで説明します。

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Original: %s\n", text);

    toLowerCase(text);
    printf("Lowercase: %s\n", text);

    return 0;
}
```

**サンプル出力:**

```
Original: Hello, World!
Lowercase: hello, world!
```

この例では、`toLowerCase`関数は入力文字列の各文字をイテレートし、`ctype.h`の`tolower`関数を使用して、それを小文字の等価物に変換します。修正は元の文字列を変更する形で行われます。

## 詳細解説

上記の例で使用される`tolower`関数は、C標準ライブラリの一部であり、具体的には`ctype.h`ヘッダーファイル内にあります。これは現在のロケールに基づいて動作しますが、標準の"C"ロケールの場合、ASCII文字セットにおいて'A'から'Z'は'a'から'z'に変換されます。

歴史的に、Cでの文字エンコーディングとケース変換の処理はASCII文字セットと密接に結びついており、ASCIIセット外の文字が一般的な国際的またはローカライズされたアプリケーションではその有用性に限界がありました。現代のプログラミング言語では、ロケールとUnicode文字を考慮してケース変換を行う組み込みの文字列メソッドを提供するかもしれませんが、Cにはネイティブにはそれがありません。

ASCII文字以外のテキスト操作、特に非ASCII文字を多用するシナリオでは、ICU（International Components for Unicode）など、より良い国際化サポートを提供するライブラリの使用を検討するかもしれません。しかし、ASCIIテキストを扱うほとんどのアプリケーションにとって、示されたアプローチは効率的で直接的です。これは、高レベル言語と比較してもう少し手間がかかるとしても、プログラマーがデータ操作をコントロールするCの傾向を示しています。
