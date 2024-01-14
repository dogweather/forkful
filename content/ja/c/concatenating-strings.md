---
title:                "C: 文字列の連結"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結を行うのはなぜでしょうか？ それは、プログラムで複数の文字列を結合する必要があるためです。例えば、ユーザーに表示するメッセージを作成するときに、動的な要素を含めたい場合や、文字列をデータベースに保存するときに必要な場合などが挙げられます。

## 方法

文字列の連結は、C言語で簡単に行うことができます。以下のようなコードを使用します。

```C
#include <stdio.h>
#include <string.h>

int main() {
  // 結合する文字列を定義する
  char firstString[] = "Hello ";
  char secondString[] = "World!";

  // 文字列の長さを取得する
  int firstStringLength = strlen(firstString);
  int secondStringLength = strlen(secondString);

  // 結合後の文字列を格納する配列を作成する
  char finalString[firstStringLength + secondStringLength];

  // 文字列を結合する
  strcat(finalString, firstString);
  strcat(finalString, secondString);

  // 結果を出力する
  printf("%s", finalString);
  return 0;
}
```

上記のコードでは、まず結合する文字列を定義し、それぞれの長さを取得します。そして、最終的な文字列を格納する配列を作成し、`strcat`関数を使って文字列を結合します。最後に、結果を出力します。

上記のコードを実行すると、以下のような結果が得られます。

```
Hello World!
```

## 深堀り

C言語の`strcat`関数は、指定された文字列を結合するだけではなく、元の文字列を変更します。つまり、結合前の文字列は破壊されてしまいます。そのため、元の文字列を保持したい場合は、`strncat`関数を使用することをお勧めします。

また、`strcat`関数が受け取る引数の文字列長には制限があり、文字列の最大長を超えた場合はプログラムが意図せず終了してしまう可能性があります。そのため、安全な結合を行うためには`strncat`関数による制限を行うことが重要です。

## 参考リンク

- [C言語の文字列の連結方法](https://programming-summary.hateblo.jp/entry/2018/06/22/075618)
- [C言語の文字列結合をマスターしよう](https://qiita.com/knmkr/items/3742d76228a37494889d)
- [strcat関数の使い方を理解しよう](https://www.sejuku.net/blog/23574)