---
title:    "Fish Shell: サブストリングの抽出"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

抽出文字列に取り組む理由は何でしょうか？
その理由は、文字列の一部だけを必要とする場合があるからです。例えば、長いファイル名から特定のキーワードだけを取り出したい場合や、テキスト処理を行う際に特定の部分だけを使用したい場合があります。抽出文字列は、これらのようなシナリオで非常に便利です。

## 方法

抽出文字列を実装するには、次のようなFish Shellのコードを使用します。

```Fish Shell
set name "John Smith"
set firstName (echo $name | cut -d " " -f1)
echo $firstName
```

このコードは、`John Smith`という文字列の一番目の単語である`John`を抽出し、出力するものです。

## ディープダイブ

抽出文字列を行うために使用されるコマンドやそのコマンドのオプションについて、もう少し詳しく見てみましょう。

#### cutコマンド

`cut`コマンドは、指定された単位で文字列を分割し、その一部分を抽出するために使用されます。デフォルトでは、タブ文字を区切り文字として使用しますが、`-d`オプションを使用することで、任意の区切り文字を指定することができます。例えば、スペースを区切り文字として使用するには、`-d " "`という風に指定します。

#### fオプション

`f`オプションは、取得したい抽出文字列の位置を指定するために使用されます。例えば、1番目の単語を抽出したい場合は、`-f1`という風に指定します。複数の単語を抽出したい場合は、`-f1-3`のように指定することもできます。

## おわりに

抽出文字列は、文字列処理を行う際に非常に役立つ機能です。是非、上記の方法を参考に、あなたのプログラミングで活用してみてください。

## おすすめリンク

- [Fish Shellユーザーズガイド（英語）](https://fishshell.com/docs/current/index.html)
- [抽出文字列を使用する場合のベストプラクティス（英語）](https://www.linux.com/training-tutorials/text-processing-using-cut/)
- [Linuxコマンドの基本（日本語）](https://www.atmarkit.co.jp/ait/articles/1708/29/news025.html)