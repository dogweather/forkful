---
title:                "Bash: 文字列の大文字化"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

Bashプログラミングを始めたばかりの人にとって、文字列を大文字に変換することは重要なスキルです。この技術をマスターすることで、より高度なスクリプトを作成することができるようになります。

## 方法

まずは、Bashシェルを開きましょう。次に、以下のコマンドを入力してください。

```Bash
str="hello world"           # 任意の文字列を設定します
echo "${str^^}"             # 大文字に変換した文字列を出力します
```

これで、単純な文字列を大文字に変換することができます。しかし、より複雑なスクリプトでは、変数やループを使用して文字列を動的に変換する必要があります。以下の例を参考にしてください。

```Bash
#!/bin/bash

echo "Enter a sentence: "    # ユーザーから文字列を入力してもらいます
read sentence
for word in $sentence       # 文字列を単語ごとに分割します
do
  echo -n "${word^} "       # 各単語の先頭を大文字に変換し、半角スペースで区切った形で出力します
done
echo                        # 改行を出力します
```

入力として「this is a sentence.」を受け取った場合、出力結果は「This Is A Sentence.」になります。

## ディープダイブ

文字列を大文字に変換する際、Bashでは主に以下の2つの方法があります。

- `${var^^}`: 変数に直接適用する方法
- `${var^}`: 変数の各単語の先頭を大文字に変換する方法

ただし、特定のロケールや文字コードを考慮する必要がある場合には、`tr`コマンドや`awk`コマンドを使用する方が適切です。また、文字列の結合や置換、切り取りなども含めて、文字列を操作する方法はさまざまあります。自分のコードで最も効率的な方法を見つけるためにも、文字列を扱うことは重要なテーマとなるでしょう。

## 詳しくはこちらを参照してください

- [Bashシェルで文字列を扱う方法](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-6.html)
- [変数と文字列の操作](https://github.com/jlevy/the-art-of-command-line/blob/master/README-ja.md#変数と文字列の操作)
- [Bashにおける文字列操作の方法](https://www.linuxjournal.com/content/bash-string-manipulation)

## 関連記事

- [Bashスクリプト入門](https://dev.to/misato-tomoyuki/a-beginners-guide-to-bash-scripting-3c8l)
- [Bashスクリプトのデバッグ方法](https://dzone.com/articles/the-art-of-debugging-bash-scripts)
- [参考になるBashチートシート](https://dev.to/awwsmm/101-bash-commands-and-tips-for-beginners-to-experts-30je)