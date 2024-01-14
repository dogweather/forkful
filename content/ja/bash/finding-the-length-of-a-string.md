---
title:                "Bash: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに興味がある理由は何でしょうか。多くの場合、プログラムの処理の中でデータを処理する必要があり、そのデータの長さを知る必要があります。特に、文字列の長さを知ることで、その文字列を他の処理や操作に利用することができます。そのため、文字列の長さを求めることは非常に重要です。

## 方法

文字列の長さを求めるには、Bashの組み込み変数である「#」を使用します。この変数を使用することで、文字列の長さを求めることができます。以下は、文字列の長さを求める方法の例です。

```Bash
# 変数に文字列を代入
str="こんにちは"

# 式を使用して文字列の長さを求める
length=${#str}

# 結果を出力する
echo "文字列の長さは $length 文字です。"

# 出力結果
# 文字列の長さは 5 文字です。
```

このように、「#」を使用することで、簡単に文字列の長さを求めることができます。

## ディープダイブ

それでは、もう少し詳しく文字列の長さを掘り下げてみましょう。実は、Bashでは「#」の代わりに「##」や「###」などのハッシュ記号を使用することもできます。「#」は一番細かい単位での文字数を表しているのに対し、「##」や「###」はそれぞれ二番目や三番目に細かい単位の文字数を表します。例えば、以下のように「###」を使用することで、文字列の最初の3文字についてのみ文字数を求めることができます。

```Bash
# 変数に文字列を代入
str="こんにちは"

# 式を使用して文字列の長さを求める
length=${##str}

# 結果を出力する
echo "文字列の最初の3文字の長さは $length 文字です。"

# 出力結果
# 文字列の最初の3文字の長さは 3 文字です。
```

このように、複数の「#」を使用することで、より詳細な文字列の長さを求めることができます。

## 併せて読みたい

- [Bashの組み込み変数について(英語)](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Bashで文字列を操作する方法 (英語)](https://www.eaton.com/content/dam/eaton/products/backup-power-ups-surge-it-power-distribution/rack-rackmount-ups/9px/pm/9px-3s-64m/linux_letter_number_aug99.pdf)
- [よく使用されるBashのコマンド (日本語)](https://qastack.jp/ubuntu/240300/earls-learn-enough)