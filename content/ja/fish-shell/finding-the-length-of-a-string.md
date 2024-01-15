---
title:                "文字列の長さを求める"
html_title:           "Fish Shell: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることの利点は、プログラミングにおいて非常に重要です。例えば、入力されたデータの長さをチェックすることで、バグを防いだり、正しい処理を行ったりすることができます。Fish Shellを使用して文字列の長さを求める方法を紹介します。

## やり方

```Fish Shell
# 文字列の長さを求めるコマンド
echo -n "Hello World" | wc -c 

# 出力： 11（改行文字が含まれないため、-nオプションを使用）
```

上記のコマンドを使用することで、文字列の長さを求めることができます。echoコマンドで出力した文字列を、wcコマンドの-cオプションを使用することで、文字数をカウントすることができます。

## 深堀り

文字列の長さを求める際には、注意点があります。例えば、改行文字を含む文字列の場合、-nオプションを使用する必要があります。また、wcコマンドはファイルの文字数をカウントすることができる他、入力された文字列の文字数をカウントすることもできます。詳しくは[wcコマンドのドキュメント](https://fishshell.com/docs/current/commands.html#wc)を参考にしてください。

## 参考リンク

- [Fish Shellドキュメント](https://fishshell.com/docs/current/index.html)
- [WCコマンドについて知ろう！](https://qiita.com/chihiro/items/0bd48eb12459418df64c)