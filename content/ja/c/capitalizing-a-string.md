---
title:                "C: 文字列の先頭を大文字にする"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##なぜ

プログラミングでは、文字列を大文字に変換することがよく行われます。大文字に変換することで、文字列を強調したり、フォーマットを一貫させたりすることができます。

##方法

以下のコードを使用して、文字列を大文字に変換することができます。

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(void) {
  char str[100];
  printf("文字列を入力してください：");
  scanf("%s",str);

  for(int i = 0; str[i]; i++){
    printf("%c", toupper(str[i]));
  }
  printf("\n");

  return 0;
}
```

入力された文字列は、大文字で出力されます。

```
文字列を入力してください：hello
HELLO
```

##深く掘り下げる

文字列を大文字に変換する際、使用する関数としては`toupper()`や`toupper_l()`があります。これらの関数は、アルファベットの小文字を大文字に変換するだけでなく、任意のロケールに対応しているので、多言語対応のプログラムにも使うことができます。

##参考リンク

- [C言語の文字列操作](https://www.javadrive.jp/cstart/cstart3/index1.html)
- [文字列を大文字に変換する方法](https://www.geeksforgeeks.org/c-program-convert-string-uppercase/)
- [C言語のtoupper/tolower関数の使い方](https://qiita.com/tacchan_d/items/a9252d6638917ede7a55)