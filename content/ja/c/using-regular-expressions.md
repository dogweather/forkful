---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
正規表現（レギュラー・エクスプレッション）は、文字列のパターンを定義するための一連の文字です。プログラマーはこれを使用して、検索、置換、データの検証などを効率的に行います。

## How to: / 方法
C言語では、`<regex.h>`ライブラリを使用して正規表現を扱います。以下は基本的な例です。

```C
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main() {
    regex_t regex;
    int ret;
    char msgbuf[100];

    // 正規表現をコンパイル
    ret = regcomp(&regex, "^a[[:alnum:]]", 0);
    if (ret) {
        fprintf(stderr, "Could not compile regex\n");
        exit(1);
    }

    // 正規表現マッチングを実行
    ret = regexec(&regex, "abc", 0, NULL, 0);
    if (!ret) {
        puts("Pattern found: abc");
    }
    else if (ret == REG_NOMATCH) {
        puts("Pattern not found");
    }
    else {
        regerror(ret, &regex, msgbuf, sizeof(msgbuf));
        fprintf(stderr, "Regex match failed: %s\n", msgbuf);
        exit(1);
    }

    // 正規表現のメモリ解放
    regfree(&regex);
    return 0;
}
```

出力例:
```
Pattern found: abc
```

## Deep Dive / 掘り下げ
正規表現は1960年代に発明されました。C言語における`<regex.h>`以外に、PCRE（Perl Compatible Regular Expressions）ライブラリなどの代替手段もあります。内部的には、正規表現ライブラリは一般に状態機械を構築してマッチング処理を行います。

## See Also / 参照
- POSIX regex documentation: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html
- PCRE library: https://www.pcre.org/
- Regular-Expressions.info tutorial: https://www.regular-expressions.info/tutorial.html
