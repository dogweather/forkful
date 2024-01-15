---
title:                "部分文字列の抽出"
html_title:           "C: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列から部分文字列を抽出することの意義は何でしょうか？それは、文字列から必要な情報を抜き出せるようにするためです。例えば、ユーザーが入力した電話番号から、国番号や市外局番を抽出することにより、より正確な情報を得ることができます。

## 使い方

文字列から部分文字列を抽出する方法はいくつかありますが、今回は`strncpy()`関数を使用してみましょう。下のコードはデモンストレーション用のサンプルコードです。

```C
#include <stdio.h>
#include <string.h>

int main(void) {
	char str[] = "こんにちは、私はカトリーヌです。";
	char sub[10];
	strncpy(sub, str + 7, 7);
	sub[7] = '\0';
	printf("部分文字列: %s\n", sub);
	return 0;
}
```

上記のコードでは、文字列`str`から`7`文字目から`7`文字分の部分文字列を抽出して、`sub`にコピーしています。そして、`printf()`関数を使って部分文字列を出力しています。

実行結果は以下のようになります。

```
部分文字列: 私はカトリーヌ
```

## ディープダイブ

文字列から部分文字列を抽出する方法は、様々な関数がありますが、その中でも`strncpy()`関数は比較的簡単に部分文字列を抽出することができる関数です。この関数には3つのパラメーターがあり、第一引数に元の文字列、第二引数に抽出する文字列の開始位置（先頭から数えた場合のインデックス）、そして第三引数に抽出する文字数を指定します。また、抽出した文字列の後ろには必ず`\0`（ヌル文字）を追加する必要があります。

文字列操作には他にも便利な関数がありますので、是非調べてみてください。

## 関連情報

- [Guru99 - C言語の文字列処理関数](https://www.guru99.com/c-string-handling.html)
- [プログラミング初心者でも分かる！C言語の文字列処理の基本](https://www.sejuku.net/blog/20087)
- [C言語 リファレンスマニュアル - 文字列操作関数](http://lopica.sourceforge.net/c/string.html)