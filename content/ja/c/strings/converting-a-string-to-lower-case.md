---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:46.569303-07:00
description: "\u65B9\u6CD5: C\u306B\u306F\u3001\u9AD8\u30EC\u30D9\u30EB\u8A00\u8A9E\
  \u306E\u3088\u3046\u306A\u6587\u5B57\u5217\u3092\u76F4\u63A5\u5C0F\u6587\u5B57\u306B\
  \u5909\u63DB\u3059\u308B\u7D44\u307F\u8FBC\u307F\u306E\u95A2\u6570\u306F\u3042\u308A\
  \u307E\u305B\u3093\u3002\u3057\u304B\u3057\u3001C\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u3001\u3053\u306E\u30D7\u30ED\u30BB\
  \u30B9\u3092\u7C21\u5358\u306B\u5B9F\u88C5\u3059\u308B\u3053\u3068\u304C\u3067\u304D\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306B\u3001\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u65B9\u6CD5\u3068\u305D\u306E\u4F8B\u3092\u30B9\u30C6\
  \u30C3\u30D7\u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u3067\u8AAC\u660E\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-04-05T21:53:43.559262-06:00'
model: gpt-4-0125-preview
summary: "C\u306B\u306F\u3001\u9AD8\u30EC\u30D9\u30EB\u8A00\u8A9E\u306E\u3088\u3046\
  \u306A\u6587\u5B57\u5217\u3092\u76F4\u63A5\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\
  \u308B\u7D44\u307F\u8FBC\u307F\u306E\u95A2\u6570\u306F\u3042\u308A\u307E\u305B\u3093\
  \u3002\u3057\u304B\u3057\u3001C\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u95A2\u6570\
  \u3092\u4F7F\u7528\u3057\u3066\u3001\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u3092\u7C21\
  \u5358\u306B\u5B9F\u88C5\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306B\u3001\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\
  \u3059\u308B\u65B9\u6CD5\u3068\u305D\u306E\u4F8B\u3092\u30B9\u30C6\u30C3\u30D7\u30D0\
  \u30A4\u30B9\u30C6\u30C3\u30D7\u3067\u8AAC\u660E\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B"
weight: 4
---

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
