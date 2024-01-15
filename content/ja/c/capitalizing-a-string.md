---
title:                "文字列のキャピタライズ"
html_title:           "C: 文字列のキャピタライズ"
simple_title:         "文字列のキャピタライズ"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列を大文字化することに興味があるかもしれません。例えば、入力された文字列をすべて大文字に変換してから処理したいときなどがあります。

## 方法
文字列を大文字化するためには、標準ライブラリの"string.h"を使用します。

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[100];
  printf("文字列を入力してください：");
  fgets(str, 100, stdin); // 文字列を標準入力から取得
  for (int i = 0; i < strlen(str); i++) {
    if (str[i] >= 'a' && str[i] <= 'z') { // 小文字の場合は大文字に変換
      str[i] = str[i] - 32; // アスキーコードでの変換
    }
  }
  printf("大文字化された文字列： %s", str); // 出力
  return 0;
}
```
プログラムを実行すると、入力した文字列が大文字化されて出力されます。例えば、"hello"と入力した場合は、"HELLO"が出力されます。

## ディープダイブ
文字列を大文字化するプロセスでは、str[i] = str[i] - 32;という処理によって、小文字のアスキーコードが大文字のアスキーコードに変換されています。小文字と大文字のアスキーコードの差は32であるため、この処理によって大文字化が可能になります。また、アスキーコードを使用することで、文字種に依存せずに大文字化ができるようになります。

## See Also
- [C言語で文字列を操作する方法](https://qiita.com/taptappun/items/235d3dca7b0351bced88)
- [ASCIIコードとUnicodeの違いについて](https://qiita.com/hmuronaka/items/a5bd88e00c7ada11d152)