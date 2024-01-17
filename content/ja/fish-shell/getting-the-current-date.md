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

# Fish Shell: プログラマーのための今日の日付を取得する方法

## What & Why?
今日の日付を取得することは、プログラミングにおいて非常によく使われる機能です。プログラマーが今日の日付を取得する理由は、例えばログやファイルの作成日時を保存するためや、特定の作業を行った日時を記録するためです。

## How to:
Fish Shellを使用して今日の日付を取得するには、以下のようなコードを使います。

```Fish Shell
date
```

これで現在の日付が表示されます。

もしくは、一部のプログラマーは日付を特定の形式で取得したい場合があります。その場合は、下記のコマンドを使用することで特定の形式で日付を取得することができます。

```Fish Shell
date +%Y%m%d
```

上記のコマンドを実行すると、例えば2022年3月14日のように日付を「年月日」の形式で取得することができます。

## Deep Dive:
今日の日付を取得する機能は、ユーザーが特定の作業を行った日時を記録することや、ファイルやログの作成日時を保存することで、後で参照するために非常に便利です。また、代替手段としてはシステムの時計を手動で調整する方法がありますが、手間がかかる上、誤差が生じる可能性もあります。

Fish Shellでは、内部的にCプログラムであるstrftime関数が使用されており、その実行にはシステムの時計を参照しています。

## See Also:
詳細なコマンドの使い方や、strftime関数については下記のソースを参考にしてください。

https://fishshell.com/docs/current/commands.html#date

もしくは、strftime関数の詳細については下記のリンクをご覧ください。

https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html