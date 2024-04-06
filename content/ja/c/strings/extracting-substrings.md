---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:40.908038-07:00
description: "\u65B9\u6CD5\uFF1A \u3044\u304F\u3064\u304B\u306E\u9AD8\u30EC\u30D9\u30EB\
  \u8A00\u8A9E\u304C\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA\u306E\u305F\u3081\
  \u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30BD\u30C3\u30C9\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u308B\u306E\u306B\u5BFE\u3057\u3001C\u8A00\u8A9E\u3067\u306F\u305D\u306E\u6587\
  \u5B57\u5217\u64CD\u4F5C\u95A2\u6570\u3092\u4F7F\u7528\u3057\u305F\u3088\u308A\u624B\
  \u4F5C\u696D\u7684\u306A\u30A2\u30D7\u30ED\u30FC\u30C1\u304C\u5FC5\u8981\u3067\u3059\
  \u3002\u6B21\u306B\u3001C\u8A00\u8A9E\u3067\u52B9\u679C\u7684\u306B\u90E8\u5206\u6587\
  \u5B57\u5217\u3092\u62BD\u51FA\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:38:42.263337-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u3044\u304F\u3064\u304B\u306E\u9AD8\u30EC\u30D9\u30EB\
  \u8A00\u8A9E\u304C\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA\u306E\u305F\u3081\
  \u306E\u7D44\u307F\u8FBC\u307F\u30E1\u30BD\u30C3\u30C9\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u308B\u306E\u306B\u5BFE\u3057\u3001C\u8A00\u8A9E\u3067\u306F\u305D\u306E\u6587\
  \u5B57\u5217\u64CD\u4F5C\u95A2\u6570\u3092\u4F7F\u7528\u3057\u305F\u3088\u308A\u624B\
  \u4F5C\u696D\u7684\u306A\u30A2\u30D7\u30ED\u30FC\u30C1\u304C\u5FC5\u8981\u3067\u3059\
  \u3002\u6B21\u306B\u3001C\u8A00\u8A9E\u3067\u52B9\u679C\u7684\u306B\u90E8\u5206\u6587\
  \u5B57\u5217\u3092\u62BD\u51FA\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\
  \uFF1A."
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## 方法：
いくつかの高レベル言語が部分文字列の抽出のための組み込みメソッドを提供しているのに対し、C言語ではその文字列操作関数を使用したより手作業的なアプローチが必要です。次に、C言語で効果的に部分文字列を抽出する方法を示します：

### 例1：`strncpy`の使用
```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Hello, World!";
    char buffer[20];

    // "Hello, World!"から"World"を抽出
    strncpy(buffer, text + 7, 5);
    buffer[5] = '\0'; // ヌル終端を確実にする

    printf("抽出された部分文字列: %s\n", buffer);
    // 出力: 抽出された部分文字列: World
    return 0;
}
```

### 例2：関数の作成
繰り返し使用する場合は、部分文字列を抽出する専用の関数を作成する方が効率的です：

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void extractSubstring(char *source, int from, int n, char *target) {
    strncpy(target, source + from, n);
    target[n] = '\0'; // ヌル終端を確実にする
}

int main() {
    char text[] = "Programming in C";
    char buffer[50];

    extractSubstring(text, 0, 11, buffer);
    printf("抽出された部分文字列: %s\n", buffer);
    // 出力: 抽出された部分文字列: Programming
    return 0;
}
```

## 深掘り
C言語での部分文字列の抽出は、主にポインタ操作と注意深いメモリ管理を通じて処理され、言語のデータを扱う低レベルのアプローチを反映しています。この方法は、Cプログラミングの初期の日々にさかのぼり、限られたコンピューティングパワーのために効率的なリソース管理が最優先事項でした。組み込みの部分文字列関数の欠如は見落としのように思われるかもしれませんが、プログラマーがメモリ管理を完全に制御することを可能にし、よく最適化されたが複雑なコードにつながるCの哲学を示しています。

現代のプログラミングの領域では、PythonやJavaScriptのような言語が`slice()`やインデックスを使用した文字列のスライスなど、部分文字列抽出のための組み込みメソッドを提供しています。これらの高レベル言語は、使いやすさと可読性と引き換えに、ある程度の制御を犠牲にしながら、メモリ管理を背景で処理します。

Cプログラマーにとって、ポインタ算術とメモリ割り当ての理解は、部分文字列の抽出のようなタスクに不可欠です。このアプローチは、文字列がメモリ内でどのように表され、操作されるかについての深い理解を必要としますが、比類のない制御と効率性を提供します。これはCプログラミングの特徴であり、性能が重要なアプリケーションで数十年にわたって関連性を保っています。しかし、直接のメモリ管理がそれほど重要でない高レベルのアプリケーションに取り組んでいる人にとっては、組み込みの部分文字列機能を持つ言語がより直接的でエラーの少ないアプローチを提供するかもしれません。
