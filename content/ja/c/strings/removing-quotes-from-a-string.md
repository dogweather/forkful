---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:29.951272-07:00
description: "\u65B9\u6CD5: C\u8A00\u8A9E\u3067\u6587\u5B57\u5217\u304B\u3089\u5F15\
  \u7528\u7B26\u3092\u524A\u9664\u3059\u308B\u306B\u306F\u3001\u5F15\u7528\u7B26\u4EE5\
  \u5916\u306E\u6587\u5B57\u3060\u3051\u3092\u65B0\u3057\u3044\u6587\u5B57\u5217\u306B\
  \u30B3\u30D4\u30FC\u3057\u306A\u304C\u3089\u6587\u5B57\u5217\u3092\u8D70\u67FB\u3057\
  \u307E\u3059\u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u3001\u6587\u5B57\u5217\
  \u306B\u5B58\u5728\u3059\u308B\u5F15\u7528\u7B26\u3059\u3079\u3066\u3092\u524A\u9664\
  \u3059\u308B\u304B\u3001\u3042\u308B\u3044\u306F\u5148\u982D\u3068\u672B\u5C3E\u306E\
  \u5F15\u7528\u7B26\u3060\u3051\u3092\u524A\u9664\u3059\u308B\u304B\u306B\u5408\u308F\
  \u305B\u3066\u8ABF\u6574\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306B\u793A\u3059\u4F8B\u306F\u3001\u3053\u308C\u3089\u306E\u30A2\u30D7\
  \u30ED\u30FC\u30C1\u306E\u4E21\u65B9\u3092\u793A\u3057\u3066\u3044\u307E\u3059."
lastmod: '2024-03-13T22:44:42.779743-06:00'
model: gpt-4-0125-preview
summary: "C\u8A00\u8A9E\u3067\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\
  \u524A\u9664\u3059\u308B\u306B\u306F\u3001\u5F15\u7528\u7B26\u4EE5\u5916\u306E\u6587\
  \u5B57\u3060\u3051\u3092\u65B0\u3057\u3044\u6587\u5B57\u5217\u306B\u30B3\u30D4\u30FC\
  \u3057\u306A\u304C\u3089\u6587\u5B57\u5217\u3092\u8D70\u67FB\u3057\u307E\u3059\u3002\
  \u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u3001\u6587\u5B57\u5217\u306B\u5B58\u5728\
  \u3059\u308B\u5F15\u7528\u7B26\u3059\u3079\u3066\u3092\u524A\u9664\u3059\u308B\u304B\
  \u3001\u3042\u308B\u3044\u306F\u5148\u982D\u3068\u672B\u5C3E\u306E\u5F15\u7528\u7B26\
  \u3060\u3051\u3092\u524A\u9664\u3059\u308B\u304B\u306B\u5408\u308F\u305B\u3066\u8ABF\
  \u6574\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\
  \u793A\u3059\u4F8B\u306F\u3001\u3053\u308C\u3089\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\
  \u306E\u4E21\u65B9\u3092\u793A\u3057\u3066\u3044\u307E\u3059."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法:
C言語で文字列から引用符を削除するには、引用符以外の文字だけを新しい文字列にコピーしながら文字列を走査します。このプロセスは、文字列に存在する引用符すべてを削除するか、あるいは先頭と末尾の引用符だけを削除するかに合わせて調整することができます。以下に示す例は、これらのアプローチの両方を示しています:

```c
#include <stdio.h>
#include <string.h>

// 文字列からすべての引用符を削除する関数
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // 目的の文字列をヌル終端
}

// 文字列から先頭と末尾の引用符だけを削除する関数
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // 目的の文字列をヌル終端
}

int main() {
    char str1[] = "'Hello, World!'";
    char str2[] = "\"Programming in C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("All Quotes Removed: %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("Edge Quotes Removed: %s\n", noQuotes2);
    
    return 0;
}
```
サンプル出力:
```
All Quotes Removed: Hello, World!
Edge Quotes Removed: Programming in C
```

これらの例は、文字列に存在するすべての引用符の削除と、先頭と末尾の引用符だけの目標とした削除の両方をどのように扱うかを示しています。

## 深掘り
文字列から引用符を削除するという概念は、C言語において、初期のテキスト処理のニーズに結びついている以外に、顕著な歴史的深みがない。ここで示された直接的なアプローチは万能ですが、非常に大きい文字列や高性能を要求される場合、その場での修正やより高度なアルゴリズムが好まれる場合があり、効率性に欠けます。

`strpbrk`を使用して引用符を見つけ、引用符でない部分の文字列を移動するような代替方法はより効率的かもしれませんが、C言語でのポインターとメモリ管理についての深い理解を要求します。さらに、正規表現ライブラリの出現は、引用符の削除を含む文字列操作のための強力なツールセットを提供してきました。しかしながら、これらのライブラリは強力ですが、単純なタスクには必要のない複雑さとオーバーヘッドを加えるかもしれません。そのため、示された直接的なアプローチは、単純さと多くの一般的な用途での効果を組み合わせた、Cプログラマーにとって価値のあるスキルのままです。
