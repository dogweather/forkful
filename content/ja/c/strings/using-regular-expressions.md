---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:03.354580-07:00
description: "\u4F7F\u3044\u65B9\uFF1A C\u3067\u6B63\u898F\u8868\u73FE\u3092\u4F7F\
  \u7528\u3059\u308B\u306B\u306F\u3001\u4E3B\u306BPOSIX regex\u30E9\u30A4\u30D6\u30E9\
  \u30EA\uFF08`<regex.h>`\uFF09\u3092\u6271\u3044\u307E\u3059\u3002\u3053\u306E\u4F8B\
  \u306F\u57FA\u672C\u7684\u306A\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\
  \u3092\u793A\u3057\u3066\u3044\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.781999-06:00'
model: gpt-4-0125-preview
summary: "C\u3067\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B\u306B\u306F\
  \u3001\u4E3B\u306BPOSIX regex\u30E9\u30A4\u30D6\u30E9\u30EA\uFF08`<regex.h>`\uFF09\
  \u3092\u6271\u3044\u307E\u3059\u3002\u3053\u306E\u4F8B\u306F\u57FA\u672C\u7684\u306A\
  \u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3092\u793A\u3057\u3066\u3044\
  \u307E\u3059\uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B"
weight: 11
---

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
