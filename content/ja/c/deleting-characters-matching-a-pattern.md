---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:41:48.650618-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字パターン一致する文字削除とは、特定のパターンにマッチする文字を文字列から取り除くことです。不要なデータのクリーンアップや、フォーマットの均一化などのためにプログラマーはこれを行います。

## How to: (方法)
```C
#include <stdio.h>
#include <string.h>

void delete_chars_matching_pattern(char *str, const char *pattern) {
    char *pr = str, *pw = str;
    while (*pr) {
        const char* pm = pattern;
        int match_found = 0;
        while (*pm) {
            if (*pr == *pm++) {
                match_found = 1;
                break;
            }
        }
        if (!match_found) *(pw++) = *(pr);
        pr++;
    }
    *pw = '\0';
}

int main() {
    char str[] = "Hello, World! 123";
    delete_chars_matching_pattern(str, "lo123");
    printf("%s\n", str); // Should print "He, Wrld! "
    return 0;
}
```

## Deep Dive (深掘り)
C言語では、文字列操作は基本的な関数群で行われますが、パターンマッチ削除は標準では提供されていません。これを実現するには、自作の関数を用意する必要があります。過去のC言語のバージョンでは正規表現ライブラリが存在しなかったこともあり、単純な文字列処理で多くのタスクが行われていました。この例では、`delete_chars_matching_pattern` 関数を用いて、指定されたパターンの文字を持つ文字列から該当する文字を削除しています。代替手段として、POSIX準拠システムでは `<regex.h>` を用いた正規表現マッチングが可能ですが、それには学習コストが伴います。

## See Also (関連情報)
- C Standard Library Functions: http://www.Ｃ標準ライブラリ.com/
- POSIX regex(3) - Linux man page: https://linux.die.net/man/3/regex
- Stack Overflow - C-related questions: https://stackoverflow.com/questions/tagged/c

Please note that some URLs may be placeholders as providing accurate and updated URLs goes beyond the capabilities of this AI. C standard library resources can be found with a search using terms like "C standard library functions" and POSIX regex documentation can be located by searching "POSIX regex manual" or accessing the appropriate system manual pages. Stack Overflow is a valuable resource for all programming questions, not just C-related ones, and can be accessed directly by visiting the provided link.