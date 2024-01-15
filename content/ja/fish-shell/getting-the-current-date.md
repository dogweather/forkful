---
title:                "現在の日付を取得する"
html_title:           "Fish Shell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得する理由はたくさんあります。例えば、ファイルの作成日や更新日を確認する場合や、タスクの期限を設定する時に必要になるかもしれません。

## 方法

まず、Fish Shellを起動し、ターミナルに以下のコードを入力します。

```Fish Shell
date
```

すると、現在の日付と時刻が表示されます。

```
Mon Aug 09 14:12:54 CDT 2021
```

また、特定の日付のフォーマットで表示したい場合は、以下のようにコードを入力します。

```Fish Shell
date "+%Y/%m/%d"
```

この場合は、実行時の年月日が表示されます。

```
2021/08/09
```

## ディープダイブ

Fish Shellでは、`date`コマンドを使用して現在の日付を取得することができます。デフォルトでは、曜日・月・日・時・分・秒・タイムゾーンの情報が表示されます。また、さまざまなオプションを使用することで、表示される情報をカスタマイズすることも可能です。詳しくは[公式ドキュメント](https://fishshell.com/docs/current/cmds/date.html)を参照してください。

また、日付だけでなく、現在の時刻のみを取得したい場合は、`date +%T`のようにコマンドを変更することもできます。

## 関連リンク

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Official GitHub Repository for Fish Shell](https://github.com/fish-shell/fish-shell)
- [Fish Shell Tutorial for Beginners](https://dev.to/muhajirdev/learn-fish-shell-the-tutorial-for-beginners-2c1m) (英語)