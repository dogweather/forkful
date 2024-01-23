---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？
標準エラー (stderr) への書き込みは、プログラムのエラーメッセージをユーザーやログファイルに出力する方法です。これは、通常の出力 (stdout) と区別して、問題を明確に伝えるために利用されます。

## How to:
## 方法:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "エラー: ファイルを開けませんでした。\n");
    return 1;
}
```
出力:
```
エラー: ファイルを開けませんでした。
```

## Deep Dive:
## 詳細:
`stderr`はUNIXの初期からあります。`stdout`とは別にエラーを扱うことで、出力をファイルにリダイレクトしてもエラーは画面に表示できます。`printf`も使えますが、`fprintf(stderr, ...)`が標準的。内部的には、`stderr`は`FILE`ポインタで、バッファリングされずに即時にフラッシュされます。

## See Also:
## 関連情報:
- C Standard Library documentation: [https://en.cppreference.com/w/c/io](https://en.cppreference.com/w/c/io)
- GNU C Library documentation on Standard Streams: [https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- POSIX standard: [https://pubs.opengroup.org/onlinepubs/9699919799/](https://pubs.opengroup.org/onlinepubs/9699919799/)
