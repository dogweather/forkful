---
title:                "新しいプロジェクトを始める"
date:                  2024-01-20T18:03:03.373907-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starting a New Project in C: A No-Nonsense Guide

## What & Why? (何となぜ？)
プロジェクトの開始って何？新しいアイディアや解決策をコードに落とし込むこと。なぜそれをするのか。新しいものを創造したり、学んだりするためだ。

## How to: (方法)
```C
#include <stdio.h>

int main() {
    printf("Hello, new project!\n");
    return 0;
}
```
実行結果:
```
Hello, new project!
```
1. テキストエディタを開く。
2. 上記のコードを書く。
3. `main.c`として保存する。
4. ターミナルでコンパイル: `gcc -o myproject main.c`
5. 実行: `./myproject`

## Deep Dive (深掘り)
始めに、1972年、C言語が登場。小さなプロジェクトからOSまで、あらゆるものが作れるようになった。他の選択肢としてはPython, Javaなどがあるが、Cの速度とコントロールは格別。プロジェクトを始める前に、いくつかの標準を学ぶべき：文字コード（UTF-8が主流）、改行コード（Unix系はLF, WindowsはCRLF）、そしてビルドシステム（Makefileなど）。

## See Also (関連情報)
- [C Programming Language (2nd Edition) by K&R](https://www.pearson.com/us/higher-education/program/Kernighan-C-Programming-Language-2nd-Edition/PGM53931.html)
- [GNU Compiler Collection (GCC)](https://gcc.gnu.org/)
- [Makefile Tutorial by Example](https://makefiletutorial.com/)
