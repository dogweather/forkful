---
title:                "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
Cプログラムで文字列を大文字にする理由はたくさんあります。例えば、データを整理する際に検索や比較をしやすくするためや、出力結果を見やすくするためです。文字列を大文字にすることで、プログラムをより強力にすることができます。

## 方法
まず、大文字にしたい文字列を選択します。次に、```toupper()```関数を使って文字列を大文字に変換します。例えば、次のように書きます。

```
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    char word[100];
    printf("文字列を入力してください：");
    gets(word);
    int i = 0;
    while (word[i]) {
        putchar(toupper(word[i]));
        i++;
    }
    return 0;
}
```

これを実行すると、入力した文字列が大文字に変換されて出力されます。例えば、入力した文字列が"hello world"の場合、出力結果は"HELLO WORLD"になります。

## 深堀り
Cプログラムでは、文字列の一部分だけを大文字にすることも可能です。例えば、```scanf()```関数を使って入力した文字列の先頭の1文字だけを大文字にし、残りの文字列はそのまま出力することもできます。

また、大文字に変換するだけではなく、小文字に変換する```tolower()```関数も存在します。これを使うことで、文字列を全て小文字にすることもできます。

## 参考
- [toupper()関数の使い方 - c言語入門](https://www.javadrive.jp/c/start/toupper/index9.html)
- [tolower()関数の使い方 - c言語入門](https://www.javadrive.jp/c/start/tolower/index9.html)
- [Cプログラムで文字列を大文字にする方法 - Qiita](https://qiita.com/zacky1972/items/ef8bbfeef9a252cf22b0)