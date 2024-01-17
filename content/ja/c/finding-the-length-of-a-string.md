---
title:                "「文字列の長さを求める」"
html_title:           "C: 「文字列の長さを求める」"
simple_title:         "「文字列の長さを求める」"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

What & Why?
文字列の長さを求めることは、プログラマーにとってよく行われるタスクです。文字列の長さを求めることは、プログラム実行中に特定の文字列を操作したり、表示したりする必要があるためです。

## How to:
```C
#include <stdio.h>
#include <string.h>

int main()
{
    // 文字列を定義する
    char str[] = "こんにちは！";
    
    // 文字列の長さを求める
    int len = strlen(str);
    
    // 結果を表示する
    printf("文字列の長さは %d です。", len);
    
    return 0;
}
```

出力：
文字列の長さは 7 です。

## Deep Dive
文字列の長さを求めることは、一見単純なタスクのように思えますが、実際にはプログラミングの基本的な要素の一つです。文字列の長さを求めることで、プログラムで扱う文字列のサイズを把握することができます。C言語では、文字列は特定の文字（ヌル文字）で終了するため、文字列の長さを正しく認識することが重要です。

文字列の長さを求める方法には、strlen()関数を使う方法以外にも、文字列を走査してヌル文字を探す方法や、専用のプログラム機能を使う方法などがあります。また、C言語以外のプログラミング言語では、文字列の長さを求めるための独自の関数が用意されている場合もあります。

## See Also
- [C言語 リファレンス：strlen関数](https://www.c-lang.org/reference/func/strlen.html)
- [C++ リファレンス：std::string::length関数](https://cpprefjp.github.io/reference/string/basic_string/length.html)
- [Java リファレンス：String.length()メソッド](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html#length--)