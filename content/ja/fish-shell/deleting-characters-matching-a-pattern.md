---
title:                "Fish Shell: パターンに一致する文字を削除する"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字のパターンにマッチする文字を削除する作業に取り組む理由は、コードをより効率的かつ簡潔にするためです。

## 方法

```fishシェルで文字のパターンにマッチする文字を削除する方法は、以下のコードを使用することで実現できます。

```Fish Shell
set string '12345abcde'
echo $string | tr -d '0-9'

# 出力: abcde
```

## ディープダイブ

文字のパターンにマッチする文字を削除するためのコーディングの一例は、`tr -d`コマンドを使用することです。このコマンドは、指定した文字のパターンにマッチする文字を削除することができます。また、`sed`コマンドや正規表現を使用することでも同様の結果を得ることができます。

## See Also（関連リンク）

- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/)
- [正規表現による文字列の置換](https://qiita.com/niisan-tokyo/items/eb78291863d477eb8cb8)