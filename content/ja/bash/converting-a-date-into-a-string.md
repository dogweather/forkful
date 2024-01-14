---
title:                "Bash: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する際の利点について説明します。日付を文字列に変換すると、より人間が読みやすい形式で日付を表示することができます。また、文字列として扱えるようになるため、日付をデータとして処理する際にも便利です。

## 方法

日付を文字列に変換するには、Bashの `date` コマンドを使用します。実際のコード例を以下に示します。

```Bash
current_date=$(date +"%m/%d/%Y")
echo $current_date
```

このコードは、現在の日付を `mm/dd/yyyy` の形式で表示します。実行結果は以下のようになります。

```Bash
06/28/2021
```

日付を別の形式に変換するには、`date` コマンドのフォーマットを変更します。詳細なフォーマットの設定方法については、`date` コマンドのマニュアルページを参照してください。

## ディープダイブ

日付を文字列に変換する際には、タイムゾーンやロケールを考慮することも重要です。`date` コマンドには、タイムゾーンやロケールを指定するオプションがあります。また、`date` コマンドの代わりに、`strftime` コマンドを使用することもできます。

さらに、日付を計算したり、特定の日付フォーマットを設定する際には、`date` コマンドだけでなく、Bashの変数や制御構文を活用することも可能です。複雑な処理を行う場合は、`date` コマンドと組み合わせることでより柔軟なコードを書くことができます。

## 参考リンク

- [dateコマンドのマニュアルページ](https://man7.org/linux/man-pages/man1/date.1.html)
- [Bashスクリプトで日付を扱う方法まとめ](https://eng-entrance.com/bash-date)
- [strftimeコマンドのマニュアルページ](https://linux.die.net/man/3/strftime)