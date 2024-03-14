---
date: 2024-01-20 17:37:19.264091-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001\u65E5\u4ED8\u306E\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3084\u3059\u3044\
  \u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30ED\u30B0\u8A18\u9332\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\
  \u30FC\u30D5\u30A7\u30FC\u30B9\u3001\u307E\u305F\u306F\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u306A\u3069\u3001\u69D8\u3005\u306A\u7406\u7531\u3067\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.263000-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001\u65E5\u4ED8\u306E\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u3084\u3059\u3044\
  \u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30ED\u30B0\u8A18\u9332\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\
  \u30FC\u30D5\u30A7\u30FC\u30B9\u3001\u307E\u305F\u306F\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u306A\u3069\u3001\u69D8\u3005\u306A\u7406\u7531\u3067\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するとは、日付のデータを読みやすいテキスト形式に変換することです。ログ記録、ユーザーインターフェース、またはデータ交換など、様々な理由でプログラマーはこれを行います。

## How to: (方法)
PHPでは`date()`や`DateTime`クラスを使って日付を文字列に変換できます。簡単な例を見てみましょう。

```php
<?php
// date() 関数を使用する方法
echo date('Y-m-d H:i:s'); // 出力例: 2023-04-05 15:30:45

// DateTime クラスを使用する方法
$dateTime = new DateTime();
echo $dateTime->format('Y-m-d H:i:s'); // 出力例: 2023-04-05 15:30:45
?>
```

このコードは、現在の日付と時刻を「年-月-日 時:分:秒」の形式で出力します。

## Deep Dive (深掘り)
日付を文字列に変換する機能は、PHPの初期バージョンから利用できます。過去には`strftime()`関数も使われていましたが、`date()`関数や`DateTime`クラスの方が強力で柔軟なため主流になりました。

- `date()`関数は秒単位のタイムスタンプを指定のフォーマットに変換します。言語設定に依存しません。
- `DateTime`クラスはオブジェクト指向で、タイムゾーンのサポートや日付の操作が容易です。`DateTime::createFromFormat()`で特定のフォーマットから日付オブジェクトを作成することもできます。

他の方法には、`IntlDateFormatter`クラスを使った国際化や、UNIX タイムスタンプを使った方法などがあります。

## See Also (関連情報)
- PHP の `date` 関数: https://www.php.net/manual/ja/function.date.php
- PHP の `DateTime` クラス: https://www.php.net/manual/ja/class.datetime.php
- PHP 日付/時刻のフォーマット: https://www.php.net/manual/ja/datetime.format.php
- PHP `IntlDateFormatter` クラス: https://www.php.net/manual/ja/class.intldateformatter.php
