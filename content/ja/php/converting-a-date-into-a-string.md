---
title:                "日付を文字列に変換する"
date:                  2024-01-20T17:37:19.264091-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
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
