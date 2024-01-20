---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ?

テキストの検索と置換は、特定の文字列を探し、必要に応じて新しい文字列で置き描えます。そしてプログラマは、コードの更新、デバッグ、およびデータの操作に役立つためこれを実施します。

## ハウツー:

```C
#include <string.h>
#include <stdio.h>

void replace_char(char *str, char find, char replace){
    for(int i = 0; i < strlen(str); i++){
        if(str[i] == find)
            str[i] = replace;
    }
}

int main(){
    char str[] = "Hello, World!";
    printf("Before: %s\n", str);
    replace_char(str, 'o', '0');
    printf("After: %s\n", str);
    return 0;
}
```

出力:

```C
Before: Hello, World!
After: Hell0, W0rld!
```

## ディープダイブ:

検索と置換は古くから存在し、古代の機械式コンピュータからデジタル時代に至るまで用いられてきました。C言語では、`strchr()` や `strstr()` のような組み込み関数を使用して検索を行うことがよくありますが、置換のための組み込み関数は存在しないため、自作することが一般的です。

また、正規表現を用いた検索や置換もよく用いられます。これらはより高度なパターンマッチングを可能にしますが、その複雑性から初学者には難解に映るかもしれません。

C言語でのテキスト検索や置換の実装では、メモリ管理に注意が必要です。不適切なメモリ管理はバグの一因となりえますので、文字列を扱う際には常に十分に注意を払うようにしましょう。

## 参照リンク:

- C Library - <string.h>: [http://www.cplusplus.com/reference/cstring/](http://www.cplusplus.com/reference/cstring/)
- Regular Expressions in C: [https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)