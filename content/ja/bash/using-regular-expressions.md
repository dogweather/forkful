---
title:                "正規表現を使用する"
html_title:           "Bash: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用するメリットは、パターンマッチングや文字列の検索、置換など、多くのテキスト処理タスクをより簡単に実行できることです。

## ハウツー

正規表現はBashプログラミングでよく使用されるテクニックです。以下の例を参考に、正規表現の基本的な使い方を学んでみましょう。

```Bash
# パターンマッチングの例
# ファイル名が"document1"で始まるものを検索する
ls | grep '^document1'

# 文字列の置換の例
# 文字列"Hello"を"こんにちは"に置換する
echo "Hello World!" | sed 's/Hello/こんにちは/'

# 数字のパターンマッチングの例
# 1-100までの数字を表示する
seq 100 | grep '[0-9][0-9]\?'

# マッチした部分を変数に代入する例
# 文字列"今日は晴れです。"から"晴れ"を抽出する
string="今日は晴れです。"
weather=$(echo $string | grep -o '晴れ')

# 条件付きパターンマッチングの例
# 1000以上の数字を表示する
echo "100 200 500 1000 1500" | grep -oE '[1-9][0-9]{3,}'
```

上記の例では、ファイル名や文字列から特定のパターンをマッチさせる方法、数字や文字列の置換をする方法、条件を指定して特定のパターンを抽出する方法など、正規表現の基本的な使い方を学ぶことができます。

## ディープダイブ

正規表現をより深く理解するには、メタ文字やクラス、グルーピングなどの概念を学ぶ必要があります。

メタ文字は、文字や単語の前にバックスラッシュを付けることで特別な意味を持つ文字です。例えば、\.はピリオドを表し、\?は直前の文字が0回または1回繰り返されることを表します。

クラスは、文字の範囲を指定するために使用されます。[0-9]は0から9までの数字を表し、[a-z]はアルファベットの小文字を表します。また、[^0-9]のように^を使うことで指定した範囲以外の文字を表すこともできます。

グルーピングは、括弧を使ってパターンをグループ化することで、部分的にマッチさせることができます。また、グルーピングを使うことで、後で参照することができる変数を作成することもできます。

## 参考リンク

- Bash正規表現チュートリアル: https://linuxconfig.org/bash-regular-expressions
- Bashの正規表現のパターン: https://www.gnu.org/software/grep/manual/grep.html#Regular-Expressions
- Bashの正規表現のクイックリファレンス: https://digit.ohtsuka-pharma.co.jp/techtips/linux/regex.html

## 参考になるリンク

- Bash学習ガイド: https://dev.to/awwsmm/learn-bash-using-these-5-resources-58lg
- Bashスクリプトのベストプラクティス: https://kvz.io/bash-best-practices.html