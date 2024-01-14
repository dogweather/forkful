---
title:                "PHP: 「未来または過去の日付の計算」"
simple_title:         "「未来または過去の日付の計算」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

将来や過去の日付を計算する理由は様々あります。例えば、何か重要なイベントの日付を把握したり、プログラムの日付に基づいて特定の処理を実行したりするためです。

## 手順

まず、PHPの date() 関数を使用して今日の日付を取得します。例えば、以下のコードを使用して今日の日付を取得することができます。

```PHP
$date = date("Y-m-d");
```

次に、date() 関数の第二引数に文字列 "strtotime" を指定し、計算したい日数を指定します。例えば、以下のように明日の日付を取得することができます。

```PHP
$tomorrow = date("Y-m-d", strtotime("+1 day"));
```

同様に、過去の日付を取得する場合は "-1 day" のように負の値を指定することで可能です。

## 深堀り

日付の計算にはさまざまなオプションがあります。例えば、特定の曜日の日付を取得したり、特定の日付から指定した日数を加算したりすることもできます。また、タイムゾーンを指定することで特定の地域の日付を取得することも可能です。

## さらに読む

[PHP 日付関数のドキュメント](https://www.php.net/manual/ja/function.date.php)

[PHP strtotime 関数のドキュメント](https://www.php.net/manual/ja/function.strtotime.php)