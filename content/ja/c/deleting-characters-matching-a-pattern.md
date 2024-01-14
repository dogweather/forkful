---
title:    "C: パターンに一致する文字を削除する"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
この記事では、C言語でパターンに一致する文字を削除する方法について説明します。プログラマーの方々にとって、文字を削除することは非常に重要なスキルです。例えば、文字列から不要な空白を削除したり、特定の文字列を取り除いたりするために使用されます。この記事では、どのようにしてパターンに一致する文字を削除できるかを詳しく紹介します。

## 作り方
文字を削除するには、配列を使用して文字列を保持する必要があります。下記は、パターンに一致する文字を削除するための基本的なコードです。

```C
#include <stdio.h>

int main(void) {
   char text[] = "Hello World";
   char pattern = 'l';
   int i, j;

   for (i = j = 0; text[i] != '\0'; ++i) {
      if (text[i] != pattern) {
         text[j++] = text[i];
      }
   }
   text[j] = '\0';
   
   printf("Result: %s", text);

   return 0;
}
```

上記のコードを実行すると、次のような結果が得られます。

```
Result: Heo Word
```

このコードでは、文字列を1文字ずつチェックし、パターンに一致する文字をスキップして新しい文字列を作成しています。

## ディープダイブ
パターンに一致する文字を削除するためのC言語のコードを書くことは、プログラミングの基本的なスキルです。しかし、より複雑なパターンマッチングが必要な場合や、大量の文字列を処理する必要がある場合はどうでしょうか。そんな時に役立つのが、正規表現と呼ばれるものです。正規表現は、特定のパターンに一致する文字列を検索や置換するための強力なツールです。C言語では、正規表現をサポートするライブラリやツールがありますので、究極的にはそれらを使用してパターンマッチングを行うことが可能です。

## それ以外にも
- [C言語の文字列操作](https://codeforwin.org/2017/12/c-programming-examples-strings.html)
- [C言語での正規表現の使用方法](https://www.thegeekstuff.com/2019/07/c-regular-expressions/)
- [正規表現を理解するための練習問題](https://regexr.com/)