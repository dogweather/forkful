---
title:                "Fish Shell: 「日付を文字列に変換する」"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

Fish Shellにおいて、日付を文字列に変換することが重要なタスクの一つです。日付を文字列に変換することで、より表現豊かな出力を得ることができます。

## ハウツー

```Fish Shell
# 今日の日付を文字列に変換する
date +"%Y年%m月%d日"
```

出力:

```
2021年10月30日
```

```Fish Shell
# 特定の日付を文字列に変換する
date -d "2 days ago" +"%Y年%m月%d日"
```

出力:

```
2021年10月28日
```

## ディープダイブ

日付を文字列に変換するためには、まずDateコマンドを使って日付を取得します。その後、文字列に変換するためには、そのDateコマンドの後ろに「+」と「フォーマットコード」を追加します。フォーマットコードは、日付を出力する際の書式を指定するものです。例えば、「%Y」は西暦の4桁を表し、```date +"%Y年"```とすることで、2021年のように出力することができます。詳しくは[Dateコマンドのマニュアル](https://fishshell.com/docs/current/commands_date.html)を参考にしてください。

## その他の情報

[日付を扱う際の便利なコマンド](https://qiita.com/terut/items/f0a9bda7f2add3caac23)を紹介するブログ記事。

[Fish Shellの日付フォーマットコード一覧](https://fishshell.com/docs/current/commands_date.html#format-codes)。