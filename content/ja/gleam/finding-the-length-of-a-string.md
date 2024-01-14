---
title:    "Gleam: 文字列の長さを見つける"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ: 文字列の長さを調べることに興味を持つ理由

文字列の長さを調べることは、プログラミング学習の基礎となる重要なスキルです。文字列の長さを計算することで、より複雑なコードを書くことができるようになります。

## 方法: Gleamで文字列の長さを調べる方法

文字列の長さを調べるには、まずGleamで文字列を作成する必要があります。それから、組み込みの`length`関数を使用して文字列の長さを求めることができます。下記のコード例を参考にしてください。

```Gleam
let string = "こんにちは！"
let string_length = length(string)
```
上記の例では、`string`変数に"こんにちは！"という文字列を代入し、`length`関数を使ってその長さを`string_length`変数に代入しています。実行すると、`string_length`の値は`6`になります。

## 詳細: 文字列の長さを調べる際の注意点

文字列の長さを調べる際に注意するべきポイントがあります。例えば、日本語のようにマルチバイト文字を含む文字列の場合、単純に文字数を数えることだけでは正確な長さが得られないことがあります。その場合は、Gleamの組み込みの`string.utf8_count()`関数を使用することでバイト数を取得することができます。

また、Gleamでは文字列はimmutable（不変）なので、文字列の一部を変更することはできません。つまり、文字列の長さを変更することもできません。

## 関連情報を見る

- [Gleamドキュメント：文字列の長さを調べる](https://gleam.run/documentation/#strings)
- [Gleamドキュメント：文字列の操作](https://gleam.run/documentation/#strings)
- [Gleam公式サイト](https://gleam.run/)

感謝します！