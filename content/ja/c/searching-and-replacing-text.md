---
title:                "C: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由は、効率的なプログラミングに不可欠な要素です。例えば、大量のテキストファイルの中から特定の文字列を検索して一括で置換することで、時間と手間を大幅に節約することができます。さらに、コンピューターの自動処理によりヒューマンエラーを防ぐこともできます。

## 方法

テキストの検索と置換を行うためには、C言語の中で使用できるいくつかの関数を利用する必要があります。例えば、"strstr"関数は、ある文字列中に特定の文字列が含まれているかどうかを判断する際に便利です。また、"strcpy"関数を使用することで、文字列の一部を別の文字列に置換することができます。以下は、テキストの置換が行われる例です。

```C
#include <stdio.h>

int main() {
    char str[] = "Hello, World!"; // 置換前の文字列
    char old[] = "Hello"; // 検索する文字列
    char new[] = "Hi"; // 置換後の文字列

    // 文字列の検索と置換
    char *ptr = strstr(str, old); // 検索する文字列が最初に現れるポインターを返す
    strcpy(ptr, new); // ポインターを置換後の文字列のポインターに書き換える

    printf("%s\n", str); // 結果を出力

    return 0;
}
```

上記のコードを実行すると、"Hi, World!"という出力が得られるはずです。

## ディープダイブ

テキストの検索と置換は、プログラミングにおいて非常に重要な作業です。しかし、一見簡単なタスクに見えても、実際には考慮しなければならないことも多くあります。例えば、大文字と小文字の区別や、複数のファイルに対する一括置換などです。さらに、文字列操作がコンピューターの動作に与える影響や、プログラムのパフォーマンスへの影響も重要なポイントです。

## See Also
- [C言語入門書](https://www.amazon.co.jp/dp/4798151139/)
- [標準Cライブラリリファレンス](https://www.ja.cp.cs.okayama-u.ac.jp/users/ichisugi/lecture/g0/StandardCLibraryReference.html)
- [文字列操作について](https://ja.wikibooks.org/wiki/C%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0/%E6%96%87%E5%AD%97%E5%88%97)