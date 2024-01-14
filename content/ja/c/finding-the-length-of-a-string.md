---
title:    "C: 「文字列の長さを見つける」"
keywords: ["C"]
---

{{< edit_this_page >}}

＃＃ なぜ
文字列の長さを調べることに取り組むのはなぜでしょうか？文字列の長さを知ることは、プログラミングで非常に便利です。例えば、文字列の操作や検証、またはプログラムのデータ解析などの作業において、文字列の長さを知ることでより効率的にプログラミングができるようになります。

## 方法

文字列の長さを調べる方法にはいくつかありますが、ここではC言語を使用した例を紹介します。

まずは、文字列変数を定義します。
```C
char str[] = "こんにちは！";
```

その後、`strlen()`関数を使用して文字列の長さを調べます。
```C
int length = strlen(str);
```
この関数は、`string.h`ライブラリで定義されており、引数に指定した文字列の長さを返します。

実行結果は以下のようになります。
```C
文字列「こんにちは！」の長さは7です。
```

その他にも、`for`ループを使用して文字列の各文字を順番に調べる方法や、`sizeof()`演算子を使用する方法などがあります。それぞれの方法について詳しくは、リンクを参考にしてみてください。

## ディープダイブ

文字列の長さを調べるためには、コンピュータ内部での文字列の扱い方を理解することが重要です。コンピュータは文字列を、各文字を表す数字の配列として処理します。そのため、文字列の長さを知るには、文字の数え上げを行うことによって実現されています。

また、その過程で文字コードや文字エンコーディングの知識が必要になる場合があります。例えば、日本語の文字を扱う場合、UTF-8やShift-JISなどの文字エンコーディングを正しく理解することが重要です。

今回紹介した`strlen()`関数は、マルチバイト文字には対応していないため、文字エンコーディングによっては正しい結果を得ることができない場合もあります。そのため、より複雑なアルゴリズムを使用する場合もありますので、さらに勉強することをお勧めします。

## 参考リンク

* [C言語のstrlen()関数の使い方](https://programming.pc-note.net/c/string/strlen.html)
* [文字列の長さを取得する方法](https://www.k-cube.co.jp/wakaba/server/strlen.html)
* [マルチバイト文字列の長さを取得する方法](https://www.mm2d.net/main/prog/c/multibyte_string_length.html)

## 参考文献

*《C言語による問題解決〜文字列処理編〜》 マッキー・マックリン 著 (翔泳社)