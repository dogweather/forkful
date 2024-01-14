---
title:                "Bash: 日期を文字列に変換する"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングにおいて、日付を文字列に変換する方法は非常に重要です。例えば、日付をデータベースに保存したり、ファイル名として使用したりする際に、特定のフォーマットの文字列が必要になるためです。

## 方法

日付を文字列に変換するには、Bashの組み込みコマンドである `date` を使用します。以下のように記述して実行することで、現在の日付が `2021年07月21日` のような形式で表示されます。

```Bash
date +"%Y年%m月%d日"
```

また、日付のフォーマットを変更することも可能です。例えば、`2021-07-21` のような形式に変更するには、次のように記述します。

```Bash
date +"%Y-%m-%d"
```

さらに、変換した日付を文字列として別の変数に保存することもできます。

```Bash
date_string=$(date +"%Y年%m月%d日")
echo $date_string
```

このように、日付を文字列に変換する方法は非常に簡単です。

## ディープダイブ

日付のフォーマットオプションには、`%Y` などの一文字のコードだけでなく、`%Y年` のように日本語を入れることも可能です。また、日付の前後に任意の文字を追加することもできます。詳細なオプションは、`man date` コマンドで確認することができます。

## 参考リンク

- [Bash dateコマンドの使い方](https://eng-entrance.com/linux-bash-date)
- [How to format date in Bash?](https://stackoverflow.com/questions/1401482)
- [Bash Date and Time (Manipulate the Output or Input) – nixCraft](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Homepage for the "date" program](https://www.gnu.org/savannah-checkouts/gnu/coreutils/manual/html_node/date-invocation.html)

## 他にも見るべきもの

[マークダウン](https://ja.wikipedia.org/wiki/Markdown)