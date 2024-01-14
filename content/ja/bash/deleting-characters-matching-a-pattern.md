---
title:    "Bash: パターンに一致する文字を削除する"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

Bashプログラミングを行う人が特定のパターンにマッチする文字を削除する理由を説明するための1-2文のみです。

文字を削除する必要がある場合、例えば特定の文字列を取り除きたい場合やファイル名を整理する必要がある場合があります。このような場合に、パターンを使用して特定の文字を削除することができます。

## 方法

文字を削除するために使用するコード例とコード内の出力を「```Bash ... ```」のコードブロックで示します。

```Bash
# 文字を削除する例
# パターン「abc」にマッチする文字を削除
# 入力: abcdefg
# 出力: defg
text="abcdefg"
pattern="abc"
echo "${text//$pattern/}"
```

`"${text//$pattern/}"`というコードを使用することで、文字列から特定のパターンにマッチする文字を削除することができます。

## 深堀り

パターンを使用して文字を削除する方法の詳細を説明します。

このコードでは、`"$pattern"`の代わりに`"${pattern}"`を使用することで、特殊な文字（例：スペース、特殊文字、ワイルドカード）をパターンとして使用することができます。また、`"${text//$pattern/}"`の代わりに`"${text//$pattern}"`を使用することで、文字列の一部ではなく全ての文字列からマッチする文字を削除することができます。

## 同じ見るもの

[The Linux Documentation Project - FAQ](https://tldp.org/LDP/abs/html/parameter-substitution.html)  
[Bash Creator's Manual: Parameter Substitution](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)