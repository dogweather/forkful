---
title:                "Fish Shell: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由は、非常に効率的であるためです。特定のテキストを複数のファイルや文書から手作業で検索して置換する作業は非常に時間がかかるため、この作業を自動化することは非常に重要です。

## 方法

テキストの検索と置換を行うには、Fish Shellの組み込みの「sed」コマンドを使用することができます。以下のようにコマンドを使用することで、特定の文字列を検索して他の文字列に置換することができます。

```Fish Shell
sed -i 's/検索するテキスト/置換するテキスト/g' ファイル名
```

このコマンドは、マッチした文字列をすべて置換するため、複数のファイルや文書を一度に処理することも可能です。また、「-i」オプションを使用することで、置換した結果を元のファイルに上書きすることもできます。

## ディープダイブ

「sed」コマンドを使用する際に注意しなければならないのは、正規表現を使用することができるという点です。正規表現を使用することで、より柔軟な検索と置換を行うことができます。例えば、特定のパターンにマッチした文字列を一括で置換することも可能です。

また、Fish Shellには「awk」コマンドもあり、こちらでもテキストの検索と置換を行うことができます。「awk」コマンドは「sed」コマンドよりも柔軟性があるため、より高度な検索と置換を行いたい場合には「sed」コマンドよりも「awk」コマンドを使用することをお勧めします。

## その他参考になるリンク

- [Fish Shellの公式ドキュメント](https://fishshell.com/docs/current/cmds/sed.html)
- [正規表現入門](https://www.atmarkit.co.jp/ait/articles/1908/09/news001.html)
- [awkコマンド入門](https://www.atmarkit.co.jp/ait/articles/1909/16/news021.html)