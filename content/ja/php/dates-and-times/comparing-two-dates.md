---
date: 2024-01-20 17:33:33.858694-07:00
description: "How to: (\u3084\u308A\u65B9) PHP\u306EDateTime\u30AF\u30E9\u30B9\u3092\
  \u4F7F\u3063\u3066\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3053\u3068\u306F\u304B\
  \u306A\u308A\u76F4\u611F\u7684\u3067\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306FPHP\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.114504-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) PHP\u306EDateTime\u30AF\u30E9\u30B9\u3092\u4F7F\u3063\
  \u3066\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3053\u3068\u306F\u304B\u306A\u308A\
  \u76F4\u611F\u7684\u3067\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306FPHP 5.2.0\u4EE5\
  \u964D\u3067\u5229\u7528\u53EF\u80FD\u306B\u306A\u308A\u307E\u3057\u305F\u3002\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3084\u53E4\
  \u3044\u95A2\u6570\uFF08`strtotime`\u306A\u3069\uFF09\u3082\u3042\u308A\u3001\u540C\
  \u3058\u76EE\u7684\u3092\u679C\u305F\u3057\u307E\u3059\u304C\u3001DateTime\u30AF\
  \u30E9\u30B9\u306F\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u6307\u5411\u306E\u30A2\u30D7\
  \u30ED\u30FC\u30C1\u3092\u63A1\u7528\u3057\u62E1\u5F35\u6027\u304C\u9AD8\u3044\u3067\
  \u3059\u3002DateTime\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u9593\u306E\u6BD4\u8F03\
  \u306F\u3001\u5185\u90E8\u3067UNIX\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u3092\
  \u4F7F\u7528\u3059\u308B\u305F\u3081\u3001\u30BF\u30A4\u30E0\u30BE\u30FC\u30F3\u306E\
  \u9055\u3044\u306B\u3082\u6B63\u78BA\u306B\u5BFE\u5FDC\u3067\u304D\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (やり方)
```PHP
<?php
$date1 = new DateTime("2023-03-01");
$date2 = new DateTime("2023-04-01");

if ($date1 < $date2) {
    echo "Date1 is earlier than Date2.";
} elseif ($date1 == $date2) {
    echo "Date1 is the same as Date2.";
} else {
    echo "Date1 is later than Date2.";
}

// Sample Output:
// Date1 is earlier than Date2.
?>
```

## Deep Dive (詳細な解説)
PHPのDateTimeクラスを使って日付を比較することはかなり直感的です。この機能はPHP 5.2.0以降で利用可能になりました。サードパーティのライブラリや古い関数（`strtotime`など）もあり、同じ目的を果たしますが、DateTimeクラスはオブジェクト指向のアプローチを採用し拡張性が高いです。DateTimeオブジェクト間の比較は、内部でUNIXタイムスタンプを使用するため、タイムゾーンの違いにも正確に対応できます。

## See Also (関連情報)
- [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
- [PHP: Date/Time Functions - Manual](https://www.php.net/manual/en/ref.datetime.php)
- [PHP: DateTime::diff - Manual](https://www.php.net/manual/en/datetime.diff.php) – 日付間の差異を計算するための関数。
