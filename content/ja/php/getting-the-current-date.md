---
title:                "PHP: 現在の日付を取得する"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ？

プログラミングを勉強している人であれば、現在の日付を取得することはよく知られたタスクです。しかし、それがなぜ重要なのか、そしてどうやって実行するのか、今回はご紹介します。

## 方法

```PHP
<?php
// 現在の日付を取得する
$currentDate = date("Y-m-d");
echo $currentDate;
// Output: 2021-02-18
```

まず、「date()」関数を使って、現在の日付を取得しましょう。この関数は指定したフォーマットで現在の日付を返します。上記の例では、「Y-m-d」を指定したため、年-月-日の形式で出力されます。

また、日時の操作や計算を行う場合は、「strtotime()」関数が便利です。例えば、今日の日付から1日後の日付を取得するには、以下のように記述できます。

```PHP
<?php
// 今日の日付から1日後の日付を取得する
$tomorrow = date("Y-m-d", strtotime("+1 day"));
echo $tomorrow;
// Output: 2021-02-19
```

さらに、「mktime()」関数を使うことで、指定した日時をタイムスタンプとして取得することができます。

```PHP
<?php
// 2021年12月25日のタイムスタンプを取得する
$timestamp = mktime(0, 0, 0, 12, 25, 2021);
echo $timestamp;
// Output: 1640371200
```

## 詳細解説

これらの関数を使うことで、プログラマーは柔軟に日時を操作することができます。また、現在の日付を取得することは、データベースのテーブルに日時を追加する際や、ブログや掲示板などの投稿でタイムスタンプを表示する際などにも重要です。

さらに、タイムゾーンを指定して日時を操作することもできます。例えば、アメリカのシアトルのタイムゾーンを指定して日時を取得するには、以下のように記述できます。

```PHP
<?php
// タイムゾーンを指定して現在の日時を取得する
date_default_timezone_set('America/Los_Angeles');
$currentDate = date("Y-m-d H:i:s");
echo $currentDate;
// Output: 2021-02-17 22:10:00
```

## 参考リンク

- [PHPのdate()関数の使い方 - 日付の書式を指定して日時を取得する方法【初心者向け】](https://techacademy.jp/magazine/31504)
- [PHPのstrtotime()で日付の加減算や任意の日時を取得する方法【初心者向け】](https://techacademy.jp/magazine/31528)
- [PHPのmktime()で日付や時間を指定してタイムスタンプを取得する方法【初心者向け】](https://techacademy.jp/magazine/31562)