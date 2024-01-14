---
title:                "C: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列から部分文字列を抽出することの利点について紹介します。

## 技術情報

抽出したい文字列のインデックスを指定することで、`substr()`関数を使用して部分文字列を簡単に取得できます。例えば、以下のコードを使用すると、文字列の4文字目から6文字目までの部分文字列を取得することができます。

```C
char str[] = "こんにちは、世界！";
char sub[4];

// 4文字目から6文字目までの部分文字列を取得
substr(sub, str, 4, 6);

// 出力は「んにち」となります
printf("%s",sub);
```

## 深堀り

文字列の部分文字列を抽出する方法について少し詳しく説明します。`substr()`関数は、指定されたインデックスから指定された長さだけ文字列を切り出します。また、C言語では、文字列を格納するバッファーの最後に`'\0'`という終端文字を加える必要があります。

文字列の長さを取得するには、`strlen()`関数を使用します。この関数は、渡された文字列の長さをバイト単位で返します。つまり、日本語の文字は2バイト分なので、文字列の長さからも部分文字列を正確に抽出することができます。

## 参考資料

[substr()関数の使用方法](https://www.programiz.com/c-programming/library-function/string.h/substr)  
[`strlen()`関数についての詳細情報](https://www.geeksforgeeks.org/strlen-function-in-c/)