---
aliases:
- /ja/php/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:32:11.363280-07:00
description: "PHP\u3067\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\
  \u3092\u8A08\u7B97\u3059\u308B\u3068\u306F\u3001\u3042\u308B\u65E5\u4ED8\u304B\u3089\
  \u7279\u5B9A\u306E\u671F\u9593\u3092\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\u3057\
  \u3066\u65B0\u3057\u3044\u65E5\u4ED8\u3092\u5F97\u308B\u3053\u3068\u3067\u3059\u3002\
  \u4E88\u7D04\u30B7\u30B9\u30C6\u30E0\u3001\u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u751F\
  \u6210\u3001\u30C7\u30FC\u30BF\u306E\u6709\u52B9\u671F\u9650\u7BA1\u7406\u306A\u3069\
  \u306E\u6A5F\u80FD\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.007490
model: gpt-4-1106-preview
summary: "PHP\u3067\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\
  \u8A08\u7B97\u3059\u308B\u3068\u306F\u3001\u3042\u308B\u65E5\u4ED8\u304B\u3089\u7279\
  \u5B9A\u306E\u671F\u9593\u3092\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\u3057\u3066\
  \u65B0\u3057\u3044\u65E5\u4ED8\u3092\u5F97\u308B\u3053\u3068\u3067\u3059\u3002\u4E88\
  \u7D04\u30B7\u30B9\u30C6\u30E0\u3001\u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u751F\u6210\
  \u3001\u30C7\u30FC\u30BF\u306E\u6709\u52B9\u671F\u9650\u7BA1\u7406\u306A\u3069\u306E\
  \u6A5F\u80FD\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
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
