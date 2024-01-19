---
title:                "文字列の大文字化"
html_title:           "C: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# C言語：文字列を大文字にする方法 

## 何と何のため？
文字列の大文字化とは、文字列中の小文字を全て大文字に変換する作業を指します。より読みやすく、検索・比較を容易にするため、プログラマーはこれを行います。

## ハウツー：
以下にC言語で文字列を大文字にする実装例とその出力を示します。

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

void capitalize(char *s) {
    for(int i = 0; s[i] != '\0'; i++){
        s[i] = toupper(s[i]);
    }
}

int main() {
    char str[] = "Hello, World!";
    capitalize(str);
    printf("%s", str); 
    return 0;
}
```

このコードを実行すると、出力は次のようになります。

```C
HELLO, WORLD!
```

## ディープダイブ：
1. 歴史的な文脈 : C言語では、ctype.hライブラリに`toupper`関数が用意されており、これを使って文字列を大文字に変換します。この関数は1970年代から存在しています。
2. 代替手段 : 大文字化を求めるより複雑な操作が必要な場合、正規表現ライブラリを使用することもあります。しかし、単純な大文字化には`toupper`関数が最も効果的です。
3. 実装の詳細 : `toupper`関数は引数として与えられた文字がアルファベットの小文字であれば該当する大文字に変換し、そうでなければそのままの文字を返します。そのため、この関数を文字列の各文字に適用することで、文字列全体を大文字に変換することができます。

## 参考情報：
以下にこの記事と関連するいくつかのソースを紹介します：

1. [C Library - <ctype.h>](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
2. [C Programming/Strings](https://en.wikibooks.org/wiki/C_Programming/Strings)
3. [C++ Reference: toupper](https://en.cppreference.com/w/c/string/byte/toupper)