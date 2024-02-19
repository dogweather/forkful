---
aliases:
- /ja/c/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:03.354580-07:00
description: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u5B9A\u7FA9\u3055\
  \u308C\u305F\u30D1\u30BF\u30FC\u30F3\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\
  \u3092\u691C\u7D22\u3001\u4E00\u81F4\u3001\u304A\u3088\u3073\u64CD\u4F5C\u3059\u308B\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\
  \u30FC\u30BF\u306E\u89E3\u6790\u3001\u5927\u304D\u306A\u30C6\u30AD\u30B9\u30C8\u30D5\
  \u30A1\u30A4\u30EB\u5185\u306E\u30D1\u30BF\u30FC\u30F3\u306E\u691C\u51FA\u306A\u3069\
  \u306E\u30BF\u30B9\u30AF\u306B\u5E83\u304F\u4F7F\u7528\u3057\u3066\u304A\u308A\u3001\
  C\u3092\u542B\u3080\u3042\u3089\u3086\u308B\u8A00\u8A9E\u3067\u5F37\u529B\u306A\u30C4\
  \u30FC\u30EB\u3068\u306A\u3063\u3066\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.339506
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u5B9A\u7FA9\u3055\u308C\
  \u305F\u30D1\u30BF\u30FC\u30F3\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\
  \u691C\u7D22\u3001\u4E00\u81F4\u3001\u304A\u3088\u3073\u64CD\u4F5C\u3059\u308B\u65B9\
  \u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\
  \u30BF\u306E\u89E3\u6790\u3001\u5927\u304D\u306A\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\
  \u30A4\u30EB\u5185\u306E\u30D1\u30BF\u30FC\u30F3\u306E\u691C\u51FA\u306A\u3069\u306E\
  \u30BF\u30B9\u30AF\u306B\u5E83\u304F\u4F7F\u7528\u3057\u3066\u304A\u308A\u3001C\u3092\
  \u542B\u3080\u3042\u3089\u3086\u308B\u8A00\u8A9E\u3067\u5F37\u529B\u306A\u30C4\u30FC\
  \u30EB\u3068\u306A\u3063\u3066\u3044\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

正規表現（regex）は、定義されたパターンを使用して文字列を検索、一致、および操作する方法を提供します。プログラマーは、入力の検証、テキストデータの解析、大きなテキストファイル内のパターンの検出などのタスクに広く使用しており、Cを含むあらゆる言語で強力なツールとなっています。

## 使い方：

Cで正規表現を使用するには、主にPOSIX regexライブラリ（`<regex.h>`）を扱います。この例は基本的なパターンマッチングを示しています：

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // 'a'で始まり英数字で続く文字列にマッチするパターン
    char *test_string = "apple123";

    // 正規表現をコンパイルする
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Could not compile regex\n");
        exit(1);
    }

    // 正規表現を実行する
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Match found\n");
    } else if (return_value == REG_NOMATCH) {
        printf("No match found\n");
    } else {
        printf("Regex match failed\n");
        exit(1);
    }

    // regexによって使用されたメモリを解放する
    regfree(&regex);

    return 0;
}
```

一致する文字列（"apple123"）のサンプル出力：
```
Match found
```
一致しない文字列（"banana"）の場合：
```
No match found
```

## 詳細解説：

Cにおける正規表現は、POSIX標準の一部として、文字列の一致と操作を実行する堅牢な方法を提供します。しかし、CのPOSIX regexライブラリのAPIは、PythonやPerlのように最初から文字列操作機能を設計した言語で見つかるAPIよりも扱いにくいと考えられています。パターンの構文は言語間で似ていますが、Cでは手動でメモリ管理を行い、regexパターンを使用する準備、実行、後処理のためにより多くのボイラープレートコードが必要です。

これらの課題にもかかわらず、Cでregexを使用する方法を学ぶことは、より低レベルのプログラミング概念の理解を深めるために有益であり、テキスト処理やデータ抽出のような分野でCプログラミングの可能性を広げます。より複雑なパターンやregex操作には、PCRE（Perl互換正規表現）ライブラリなどの代替手段が、より多くの機能を備え、多少使いやすいインターフェースを提供するかもしれませんが、Cプロジェクトに外部ライブラリを統合する必要があります。
