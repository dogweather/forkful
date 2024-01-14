---
title:    "C: 文字列のキャピタライズ"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why (なぜ):
文字列の大文字化を行う理由を1〜2文で説明します。

文字列の大文字化とは、文字列内のすべての文字を大文字に変換することであり、主に文字列の比較や表示の統一性を保つために使用されます。例えば、データベース内の名前を大文字で統一することで、同じ名前を持つレコードを正しく識別できるようになります。

## How To (方法):
以下の「```C ... ```」コードブロック内に、文字列を大文字化するためのプログラム例と実行結果を示します。

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// 文字列を大文字に変換する関数
void capitalize_string(char* str) {
  int i = 0;
  while (str[i]) {
    str[i] = toupper(str[i]); // toupper()関数で文字を大文字に変換
    i++;
  }
}

int main() {
  char name[20] = "yamada taro"; // 変換前の文字列
  printf("Before: %s\n", name); // 変換前の文字列を表示
  capitalize_string(name); // 大文字化関数を呼び出し
  printf("After: %s\n", name); // 変換後の文字列を表示
  return 0;
}
```

実行結果:

```C
Before: yamada taro
After: YAMADA TARO
```

## Deep Dive (詳細):
文字列の大文字化には、さまざまな方法がありますが、上記のプログラムは文字列を一文字ずつ処理していることが特徴です。ここで使用している「toupper()」関数は、文字のASCIIコードを使用して大文字に変換しています。

また、文字列の大文字化には「strtoupper()」関数や「strlwr()」関数なども使用できます。これらの関数は、文字列を一括で大文字化や小文字化することができます。しかし、これらの関数はロケールによって変換する文字が異なる場合があるため、注意が必要です。

## See Also (関連記事):
- [C言語での文字列操作](https://qiita.com/nametaketaketake/items/8c63ac9989f9c6b54fe7)
- [toupper()関数の使い方](https://programming.pc-note.net/c/character.html#toupper)

以上で、C言語で文字列を大文字化する方法を紹介しました。大文字化は、データを正しく識別するために欠かせない処理であるため、正しく理解しておくことが重要です。ご参考になれば幸いです。