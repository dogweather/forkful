---
title:                "Bash: 文字列の先頭を大文字に変換する"
simple_title:         "文字列の先頭を大文字に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##なぜ

「Bash」プログラミングを学習する理由はたくさんありますが、今回は「文字列の大文字化」を取り上げます。文字列の大文字化は、プログラミングの世界で非常に役立つスキルであり、テキスト処理やデータ操作など多くの場面で必要になるからです。

##やり方

「文字列の大文字化」を実現する方法はいくつかありますが、ここでは「tr」コマンドを使用した方法を紹介します。

まず、任意の文字列を定義します。

```Bash
str="hello world"
```

次に、`tr`コマンドを使用して大文字化します。

```Bash
echo $str | tr '[:lower:]' '[:upper:]'
```

実行すると、出力結果は`HELLO WORLD`となります。

また、変数などの値を大文字化する場合は、以下のように`tr`コマンドと組み合わせて使用します。

```Bash
value="apple"
echo $value | tr '[:lower:]' '[:upper:]'
```

出力結果は`APPLE`となります。

##詳細な説明

「文字列の大文字化」を実現するために使用する`tr`コマンドは、テキスト変換ユーティリティの1つであり、文字の置換や削除、変換などを行うことができます。引数として受け取った文字列の文字コードを示すオプションを使用することで、大文字や小文字などの変換を行うことができます。

詳細な使用方法やオプションについては、公式ドキュメントを参考にしてください。

##参考リンク

- [Bash 公式ドキュメント](https://www.gnu.org/software/bash/)
- [tr コマンドのチュートリアル](https://linux.die.net/man/1/tr)
- [文字列の大文字化について](https://ja.wikipedia.org/wiki/%E5%A4%A7%E6%96%87%E5%AD%97)