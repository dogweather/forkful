---
title:                "Fish Shell: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することについて、なぜそれが重要なのかを簡単に説明します。

## How To

ファイルを開き、日付を文字列に変換するためのFish Shellのコード例とサンプルの出力をご紹介します。

```
Fish Shell $ set date (date +%Y-%m-%d)
Fish Shell $ echo $date
```

このコードを実行すると、現在の日付が「2021-07-21」という文字列に変換されます。

## Deep Dive

どのようにして日付が文字列に変換されるのかを詳しく説明します。日付は、プログラムでは数字として扱われますが、ユーザーにとっては文字列として読みやすい表示が必要です。Fish Shellの `set` コマンドを使用することで、文字列への変換が可能になります。 `date` コマンドを使用することで、現在の日付を取得し、その後に `%Y-%m-%d` を続けることで、指定した形式の文字列に変換することができます。

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Date Command in Fish Shell](https://fishshell.com/docs/current/commands.html#date)