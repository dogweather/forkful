---
title:                "日付を文字列に変換する"
html_title:           "Fish Shell: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することに取り組む理由は、日付の形式を自分のニーズに合わせやすくするためです。例えば、ファイル名に日付を含めたい場合や、データベースに日付を保存する際に文字列として扱いたい場合などがあります。

## 方法

日付を文字列に変換するには、Fish Shellの幾つかの便利なコマンドを使用します。例として、今日の日付を文字列で出力する方法を以下のように示します。

```Fish Shell

# `date -u`コマンドで現在の日付をUTCで取得し、`string join`コマンドで"/"で区切った文字列に変換
echo (date -u | string join "/")

# 出力例：2020/10/01
```

他にも、`strftime`コマンドを使用することで、より細かい日付のフォーマットを指定できます。例えば、"年-月-日"の形式で出力する場合は、以下のようにコードを書き換えます。

```Fish Shell

# `strftime`コマンドで日付を指定した形式で出力
echo (date -u | strftime "%Y-%m-%d")

# 出力例：2020-10-01
```

## ディープダイブ

日付を文字列に変換する際には、文字列のフォーマットを指定することが重要です。Fish Shellでは、`strftime`コマンドを使用することで、自由にフォーマットを指定することができます。詳しい使い方やサポートしているフォーマットの一覧は、[公式ドキュメント](https://fishshell.com/docs/current/cmds/strftime.html)を参照してください。

## 参考リンク

- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHubリポジトリ](https://github.com/fish-shell/fish-shell)
- [Fish Shellを使いこなそう！初心者向けのチートシート](https://dev.classmethod.jp/articles/fish-shell-cheat-sheet/)