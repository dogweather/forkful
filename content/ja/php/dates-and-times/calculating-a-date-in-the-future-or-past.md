---
title:                "将来または過去の日付を計算する"
aliases:
- /ja/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:11.363280-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
PHPで将来または過去の日付を計算するとは、ある日付から特定の期間を加算または減算して新しい日付を得ることです。予約システム、リマインダー生成、データの有効期限管理などの機能を実現するためにプログラマーはこれを行います。

## How to: (方法)
```PHP
<?php
// 現在の日付を取得します
$today = new DateTime();

// 10日後の日付を計算します
$interval = new DateInterval('P10D');
$futureDate = $today->add($interval);
echo $futureDate->format('Y-m-d'); // 出力: 10日後の日付

// 10日前の日付を計算します
$interval = new DateInterval('P10D');
$pastDate = $today->sub($interval);
echo $pastDate->format('Y-m-d'); // 出力: 10日前の日付
?>
```

## Deep Dive (詳細な掘り下げ)
PHPで日付計算をするとき、`DateTime`クラスがよく使われます。これは、PHP 5.2.0以降で利用可能です。`DateInterval`クラスを使って期間を指定することで、非常に直感的に日付を加算または減算できます。この方法は、タイムゾーンの管理や夏時間などの複雑なケースを扱う際にも強力です。

過去には、`strtotime()`関数と`date()`関数の組み合わせもよく使用されましたが、この方法は`DateTime`クラスに比べて少し非直感的かつ誤りやすい部分がありました。例えば、`strtotime()`は非常に強力ですが、複雑な日付計算では想定しない結果を返すことがあります。

実装の詳細を理解することは、サマータイムの影響をはじめとするトラブルに対処する際に有用です。例えば、日付を跨ぐサマータイムの切り替えの際に正しい計算が行われるようにするためです。

## See Also (関連情報)
- PHPの公式マニュアルのDateTimeクラス: https://www.php.net/manual/en/class.datetime.php
- DateIntervalクラスの詳細: https://www.php.net/manual/en/class.dateinterval.php
- strtotime()関数の使用方法: https://www.php.net/manual/en/function.strtotime.php
- 日付と時刻の処理に関連するPHPの関数一覧: https://www.php.net/manual/en/book.datetime.php
