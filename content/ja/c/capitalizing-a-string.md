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

下記の「What & Why?」から「See Also」までの記事

## What & Why?
文字列を大文字にするとは、プログラマーが文字列の各文字を大文字に変換することです。プログラマーがこれをする理由は、簡単に言うと、文字列を比較したり、特定の条件に合致するかどうかをチェックするためです。

## How to:
```C
// 文字列を大文字に変換する関数
void to_uppercase(char *str){

    // 文字列の長さを取得
    int length = strlen(str);

    // 各文字をチェックし、小文字の場合は大文字に変換
    for(int i = 0; i < length; i++){
        if(str[i] >= 'a' && str[i] <= 'z'){
            str[i] -= 32;
        }
    }
    
    // 変換後の文字列を出力
    printf("変換後の文字列: %s", str);
}

int main(){

    // 大文字に変換する文字列
    char str[] = "programming";

    // 変換前の文字列を出力
    printf("変換前の文字列: %s\n", str);

    // 大文字に変換する関数を呼び出し
    to_uppercase(str);
    
    return 0;
}
```
出力：
変換前の文字列: programming
変換後の文字列: PROGRAMMING

## Deep Dive:
文字列を大文字に変換する機能は、プログラミング言語によって実装方法が異なります。C言語では、文字列を配列として扱い、各文字をASCIIコードを用いて変換することで実現します。この機能は、プログラムが文字列比較や特定の文字列での操作を行う際に大きな役割を果たします。また、C言語以外の言語においても、同様の機能を備えている場合があります。

## See Also:
- [ASCIIコード表](https://ascii.jp/elem/000/000/032/32/)
- [文字列比較の仕組みについて](https://www.it-collage.jp/programming/programming_basic08.html)