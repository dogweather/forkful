---
title:                "「パターンにマッチする文字を削除する」"
html_title:           "Fish Shell: 「パターンにマッチする文字を削除する」"
simple_title:         "「パターンにマッチする文字を削除する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

パターンにマッチする文字を削除することに興味がある人はどんな理由があるのでしょうか？その答えは、例えばテキストデータの整理や特定の文字を除外する必要があったり、文字列処理において特定のパターンを取り除く必要があるからかもしれません。

## 方法

Fish Shellを使ってパターンにマッチする文字を削除するには、まず「grep」コマンドを使用してパターンにマッチする行を抽出し、その後「sed」コマンドを使用して不要な文字を削除します。

```Fish Shell
grep "パターン" ファイル名 | sed 's/削除する文字//g'
```

このコードを実行すると、パターンにマッチする行から不要な文字が削除された状態の出力が得られます。

## 深堀り

「grep」コマンドは、指定したパターンにマッチする行を抽出し、それ以外の行を除外することができます。また「sed」コマンドは、文字列の置換や削除などの文字列処理をすることができます。上記のコードでは、「マッチした文字列を抽出した後、その行から不要な文字を削除する」という流れになります。

## 参考リンク

- [Fish Shell 公式ドキュメント](https://fishshell.com/docs/)
- [grepコマンドについて知る](https://techplay.jp/column/645)
- [sedコマンド基本運用法](https://qiita.com/take4s5i/items/a0ca9929336634277e62)