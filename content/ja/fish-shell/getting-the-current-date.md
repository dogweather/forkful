---
title:                "Fish Shell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得するために何ができるのか、その理由を知りたいと思いませんか？Fish Shellプログラミングにおいて、現在の日付を取得することは非常に重要です。この記事では、なぜそのように重要なのかをご紹介します。

## 方法

現在の日付を取得するには、`date`コマンドを使用します。以下のようにコマンドを入力すると、現在の日付が表示されます。

```Fish Shell
date
```

サンプル出力は以下のようになります。

```
Thu Nov 4 15:30:59 JST 2021
```

また、`date`コマンドには様々なオプションがあります。例えば、日付のフォーマットを指定することもできます。以下のようにコマンドを入力すると、現在の日付を`yyyy-mm-dd`の形式で表示することができます。

```Fish Shell
date +%Y-%m-%d
```

サンプル出力は以下のようになります。

```
2021-11-04
```

## 深堀り

`date`コマンドのような日付取得のためのコマンドは、システムで使用される時刻を表示するためにも使われます。さらに、日付と時間の計算や操作も可能です。

また、日付を取得するためにはシステムの時刻が正確であることが重要です。そのため、日付を取得する前にシステムの時刻を同期させることを推奨します。

## 関連リンク

- [Fish Shellのホームページ](https://fishshell.com/)
- [Fish Shellのドキュメント](https://fishshell.com/docs/current/index.html)
- [dateコマンドのドキュメント](https://fishshell.com/docs/current/cmds/date.html)
- [初めてのFish Shell](https://qiita.com/yugui/items/b719f0f5f5d9fccb9c1c)
- [Fish Shellでのシェルスクリプト入門](https://qiita.com/hhmlab/items/a48d4206d3542738da09)