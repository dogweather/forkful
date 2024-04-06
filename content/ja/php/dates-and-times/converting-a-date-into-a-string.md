---
date: 2024-01-20 17:37:19.264091-07:00
description: "How to: (\u65B9\u6CD5) PHP\u3067\u306F`date()`\u3084`DateTime`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u3063\u3066\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\
  \u63DB\u3067\u304D\u307E\u3059\u3002\u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\
  \u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.113564-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PHP\u3067\u306F`date()`\u3084`DateTime`\u30AF\u30E9\u30B9\
  \u3092\u4F7F\u3063\u3066\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3067\
  \u304D\u307E\u3059\u3002\u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

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
