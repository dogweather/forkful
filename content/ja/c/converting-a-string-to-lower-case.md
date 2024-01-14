---
title:    "C: 「文字列を小文字に変換する」"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換するのはなぜか？一言で説明すると、プログラム内で文字列を一貫して扱うために必要です。

## 使い方
まず、文字列を格納するための変数を定義します。次に、その変数に対して `tolower()` 関数を使用します。以下は、実際のコーディング例です。

```C
char str[] = "HELLO WORLD";
int i;

// 文字列を小文字に変換
for (i = 0; str[i]; i++) {
  str[i] = tolower(str[i]);
}

printf("変換後の文字列は %s です", str);

/* Output:
変換後の文字列は hello world です
*/
```

## 深堀り
文字を小文字に変換するためには、ASCIIコード表を参照する必要があります。ASCIIコード表では、大文字と小文字の間には32という数値の差があります。そのため、`A` を `tolower()` 関数に渡すと、`a` が返される仕組みです。

また、`tolower()` 関数は `ctype.h` ヘッダーファイルに含まれています。そのため、文字列を変換する前に、適切なライブラリをインクルードする必要があります。

## 参考リンク
- [C言語の文字列を操作する方法](https://w.atwiki.jp/uta1008/pages/5.html)
- [ASCIIコード表](https://ascii.jp/)
- [ctype.hヘッダーファイルについて](https://www.jisilu.cn/question/97487)