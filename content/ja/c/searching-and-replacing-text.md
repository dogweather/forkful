---
title:                "テキストを検索して置換する"
html_title:           "C: テキストを検索して置換する"
simple_title:         "テキストを検索して置換する"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索や置換を行いたい理由は、コンピュータプログラムで大量のテキストファイルを処理する必要があるからです。テキストの検索や置換は、繰り返し行う必要がある作業であり、それを手作業で行うと非常に時間がかかり、ミスも起こりやすいため、プログラマーにとっては重要なスキルです。

## 方法

テキストの検索や置換は、C言語で簡単に行うことができます。まず、string.hヘッダーファイルをインクルードする必要があります。

```C
#include <string.h>
```

次に、strchr関数を使って文字列内で特定の文字を検索することができます。例として、文字列"Hello World!"内で文字"o"を検索し、その位置を表示するコードは次のようになります。

```C
char *str = "Hello World!";
char *p = strchr(str, 'o');
printf("Position: %d", p - str);
```

上記のコードの出力は、"Position: 4"となります。

また、文字列内の特定の文字を別の文字列に置換するには、strtok関数を使うことができます。例として、文字列"Hello World!"内のすべての"o"を"x"に置換するコードは次のようになります。

```C
char *str = "Hello World!";
char *p = strtok(str, "o");
while (p != NULL) {
    printf("%s", p);
    p = strtok(NULL, "o");
}
```

上記のコードの出力は、"Hellx Wxrld!"となります。

## 深堀り

テキストの検索や置換は、C言語の標準ライブラリであるstring.hに含まれる関数を使うことで、簡単に行うことができます。strchr関数やstrtok関数以外にも、strstr関数やstrncpy関数など、様々な関数がありますので、必要に応じて調べてみてください。

また、検索や置換を行うだけでなく、文字列の比較や連結もC言語では簡単に行うことができます。これらの関数を使いこなすことで、より複雑な操作や処理を行うことができますので、積極的に活用してください。

## もっと詳しく知りたい方へ

- [C言語のstring.hヘッダーについて](https://www.ibm.com/support/knowledgecenter/ja/ssw_ibm_i_72/rtref/strchr.htm)
- [string.hの関数一覧](https://ja.wikipedia.org/wiki/String.h#.E5.87.BA.E5.8A.9B)
- [C言語での文字列操作について](https://www.programming-techniques.com/2013/02/c-how-to-use-string-library-functions.html)

## 関連リンク

- [C言語の基礎知識](https://qiita.com/tags/C%E8%A8%80%E8%AA%9E)
- [C言語での文字列操作の基本](https://www.techscore.com/tech/C/Basic/Basic0_2/)