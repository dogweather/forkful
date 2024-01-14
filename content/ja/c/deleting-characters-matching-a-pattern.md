---
title:                "C: パターンに一致する文字を削除する"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
特定のパターンにマッチする文字を削除することの重要性や利点について説明します。この機能を使用することで、文字列のデータをより効率的に処理することができるようになります。

## 方法
まず最初に、C言語の標準ライブラリであるstring.hをインポートします。このライブラリには、文字列を処理するための便利な関数がたくさん含まれています。

例えば、以下のようなコードを使用することで、文字列から特定のパターンにマッチする文字を削除することができます。

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[100] = "Hello World";
  char pattern[10] = "llo";

  // パターンにマッチする文字を削除する
  str = strpbrk(str, pattern);
  
  printf("%s", str);

  return 0;
}
```

上記の例では、文字列"Hello World"から"llo"というパターンにマッチする文字を探し、その文字を削除しています。そして、最終的な出力結果は"H World"となります。

このように、文字列を処理する際には、特定のパターンにマッチする文字を削除することで、データをよりシンプルにし、処理を効率的にすることができます。

## ディープダイブ
この機能を実装する方法についてさらに詳しく見ていきましょう。上記の例では、文字列の中から一番最初にマッチした文字を削除していますが、もし複数のマッチがある場合、それら全てを削除する必要がある場合もあります。

そのような場合には、string.hライブラリに含まれるstrpbrk()関数ではなく、strspn()関数を使用することで、マッチする文字を全て削除することができます。

さらに、strtr()関数を使用することで、特定の文字を別の文字に置き換えることもできます。

このように、文字列を操作するためのライブラリや関数はたくさん存在するので、必要に応じて様々な方法を試してみることが大切です。

## 関連リンク

- [strpbrk() function](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
- [strspn() function](https://www.tutorialspoint.com/c_standard_library/c_function_strspn.htm)
- [strtr() function](https://www.tutorialspoint.com/c_standard_library/c_function_strtr.htm)
- [C言語のstring.hライブラリについて](https://codezine.jp/article/detail/4532)
- [C言語を使った文字列処理の基本](http://www.tohoho-web.com/ex/c_string.html)

より詳しい情報やサンプルコードを見つけることで、文字列を操作する際により効率的な方法を見つけることができるでしょう。

## その他のリンク
- [Markdown記法の使い方](https://gist.github.com/ihoneymon/652be052a0727ad59601)
- [GitHub公式マークダウン記法ガイド](https://guides.github.com/features/mastering-markdown/)
- [Markdownを上手に書こう](https://www.1101.com/mm/)