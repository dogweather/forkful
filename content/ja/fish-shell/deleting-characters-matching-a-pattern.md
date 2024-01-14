---
title:                "Fish Shell: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ削除するのか: パターンにマッチする文字を削除する理由

Fish Shellを使用している人々は、ファイルやテキスト内の特定の文字を削除したい場合があります。しかし、手動でそれらの文字を一つずつ削除するのは面倒です。そこで、パターンにマッチする文字を一括で削除する方法が注目されています。

## 方法: `Fish Shell`コードブロックでのコーディング例とサンプル出力

```
# パターンにマッチする文字を削除する例

$ set fruits apple banana pear watermelon
$ echo $fruits
apple banana pear watermelon

# "a"という文字にマッチするものを削除する
$ echo $fruits | tr -d a
ppl bnn perr wtermelon
```

## 深堀: パターンにマッチする文字を削除する方法の詳細

`tr`コマンドを使用することで、パターンにマッチする文字を削除することができます。また、`sed`や`awk`といった他のコマンドでも同様の結果を得ることができます。しかし、Fish Shellでは独自の構文を使用することで、より簡単に文字を削除することができます。

## もっと見る: 
- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html) 
- [Bash Shellとの違いを知る](https://qiita.com/wellflat/items/2341c40b8160f4dbb12e)