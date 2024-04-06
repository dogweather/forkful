---
date: 2024-01-20 17:32:11.363280-07:00
description: "How to: (\u65B9\u6CD5) PHP\u3067\u65E5\u4ED8\u8A08\u7B97\u3092\u3059\
  \u308B\u3068\u304D\u3001`DateTime`\u30AF\u30E9\u30B9\u304C\u3088\u304F\u4F7F\u308F\
  \u308C\u307E\u3059\u3002\u3053\u308C\u306F\u3001PHP\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.793207-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PHP\u3067\u65E5\u4ED8\u8A08\u7B97\u3092\u3059\u308B\u3068\
  \u304D\u3001`DateTime`\u30AF\u30E9\u30B9\u304C\u3088\u304F\u4F7F\u308F\u308C\u307E\
  \u3059\u3002\u3053\u308C\u306F\u3001PHP 5.2.0\u4EE5\u964D\u3067\u5229\u7528\u53EF\
  \u80FD\u3067\u3059\u3002`DateInterval`\u30AF\u30E9\u30B9\u3092\u4F7F\u3063\u3066\
  \u671F\u9593\u3092\u6307\u5B9A\u3059\u308B\u3053\u3068\u3067\u3001\u975E\u5E38\u306B\
  \u76F4\u611F\u7684\u306B\u65E5\u4ED8\u3092\u52A0\u7B97\u307E\u305F\u306F\u6E1B\u7B97\
  \u3067\u304D\u307E\u3059\u3002\u3053\u306E\u65B9\u6CD5\u306F\u3001\u30BF\u30A4\u30E0\
  \u30BE\u30FC\u30F3\u306E\u7BA1\u7406\u3084\u590F\u6642\u9593\u306A\u3069\u306E\u8907\
  \u96D1\u306A\u30B1\u30FC\u30B9\u3092\u6271\u3046\u969B\u306B\u3082\u5F37\u529B\u3067\
  \u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
