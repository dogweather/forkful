---
title:                "Bash: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を連結することの理由について、たった1〜2文で説明します。

### 文字列を連結するとは
文字列の連結とは、2つ以上の文字列を結合して1つの文字列にする処理のことです。文字列の連結は、多くのプログラミング言語で一般的に使われており、Bashでも同様です。例えば、あなたが友達に挨拶する際に「こんにちは、名前！」と言うとします。この場合、"こんにちは"という文と名前を結合して1つの文にすることができます。

## 方法
文字列を連結する方法について、以下のコードブロックを使ってコーディング例と出力のサンプルを示します。

```Bash
# 文字列を連結する方法
name="太郎"
greeting="こんにちは、"
echo "${greeting}${name}！"
```
出力結果: こんにちは、太郎！

上記の例では、変数を使用して文字列を定義し、echoコマンドで変数を結合して出力しています。

別の方法として、文字列を直接結合することもできます。

```Bash
# 文字列を直接連結する方法
echo "おはよう、" "太郎！"
```
出力結果: おはよう、太郎！

## 深堀り
文字列の連結には、他の言語と比べてBashには独自の特徴があります。Bashでは、単純に文字列を結合するだけでなく、変数や数値などを含めた複雑な式の結果を文字列に結合することもできます。

また、シングルクォートやダブルクォートなど、結合する文字列の外側に使用する引用符が出力結果に影響することもあります。そのため、注意して使用する必要があります。

## 関連記事
Bashで文字列を連結することの重要性や様々なテクニックについて学ぶことができました。以下のリンクを参考にして、さらにBashのプログラミングスキルを向上させましょう！

# 関連記事
- [Bash scripting 101](https://www.linode.com/docs/guides/bash-scripting-101/)
- [How to concatenate string variables in Bash](https://linuxconfig.org/how-to-concatenate-string-variables-in-bash)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/string-manipulation.html)