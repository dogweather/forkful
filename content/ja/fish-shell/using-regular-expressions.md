---
title:                "正規表現の利用"
html_title:           "Fish Shell: 正規表現の利用"
simple_title:         "正規表現の利用"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

なぜ正規表現を使うことが重要なのか、知っていますか？正規表現は特定のパターンを検索、マッチング、置換するために使われる強力なツールです。例えば、特定の文字列を含むファイルを一括で置換する必要がある場合、正規表現を使うことで簡単に実現することができます。

## 使い方

まずはFish Shellにおける正規表現の基本的な書き方を紹介します。例として、任意のファイル名に"hello"という文字列が含まれているかどうかを確認するコマンドを作成してみましょう。

```
Fish Shellでは、正規表現は=//で囲まれた部分に記述します。
```

```
# 確認するフォルダに移動する
cd my_folder
```

```
# "hello"という文字列を含むファイル名があるかを確認する
fish -c "count -x 'hello' *.txt"
```

```
=//の中に含まれるパターンを変更することで、より詳細な検索が可能です。
```

```
おめでとうございます！正規表現を使ってファイル名に"hello"が含まれているファイルの数が表示されました。
```

## ディープダイブ

さらに深く正規表現を理解するために、パターンの記号や特殊文字についての詳細を説明します。例えば、以下のような正規表現を考えてみましょう。

```
[0-9]+
```

このパターンは、数字が連続していることを表しています。つまり、1つ以上の数字が続く文字列にマッチします。具体的な例を挙げると、"123"や"5"という文字列にマッチします。また、特殊文字として使われるバックスラッシュ（\）を用いることで、普段は特別な意味を持つ記号を普通の文字として扱うことができます。

詳しい正規表現の記法については、[正規表現チートシート](https://www.cheatography.com/davechild/cheat-sheets/regex/)を参考にしてください。

## 今後も参考になる情報

もし、もっと詳しく正規表現について学びたい場合は以下のリンクを参考にしてください。

- [Fish Shellドキュメント](https://fishshell.com/docs/current/)
- [正規表現チートシート](https://www.cheatography.com/davechild/cheat-sheets/regex/)
- [正規表現の練習サイト (RegexOne)](https://regexone.com/)

## もっと楽しもう！

正規表現は、一見難しく感じるかもしれませんが、慣れると非常に便利なツールです。ぜひこの記事を参考に、実際にFish Shellで正規表現を使ってみてくださいね！