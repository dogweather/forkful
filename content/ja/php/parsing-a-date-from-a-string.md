---
title:                "文字列から日付を解析する"
html_title:           "PHP: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何をして、なぜプログラマーがそれをするのか？

日付を文字列から解析することは、プログラムで日付を処理する際に非常に重要です。これにより、ユーザーが入力した日付の形式を統一し、正しい日付データを取得することができます。プログラマーが日付を解析する理由は、日付をデータベースに保存したり、計算したりする必要があるためです。

## 方法：

日付を解析するには、PHPの標準関数である`date_parse()`を使用します。この関数は、指定した日付文字列を様々な部分に分解し、連想配列として返します。

例えば、"2021-01-01"という文字列を解析すると、以下のような結果が返ってきます。

```PHP
$date = date_parse("2021-01-01");

print_r($date);

/*
Array
(
    [year] => 2021
    [month] => 1
    [day] => 1
    [hour] => 
    [minute] => 
    [second] => 
    [fraction] => 
    [warning_count] => 0
    [warnings] => Array
        (
        )

    [error_count] => 0
    [errors] => Array
        (
        )

    [is_localtime] => 
)
*/
```

上記のように、年、月、日などの値が個別に取得できます。

## 解説

日付を文字列から解析する方法は古くから存在しており、プログラマーにとっては基本的な知識です。ただし、PHPの`date_parse()`以外にも、ライブラリや外部ツールを使用することでさらに詳細な情報を取得することが可能です。

例えば、PHPのDateTimeクラスを使用すると、解析した日付からさらに時刻やタイムゾーンなどの情報を取得することができます。また、外部ツールとしてはCarbonやMoment.jsなどがあります。

日付を文字列から解析する際に重要なのは、文字列のフォーマットを正しく理解することです。日付には様々なフォーマットがあり、それぞれの場合に応じた処理が必要になります。

## 関連リンク

- [PHPの公式ドキュメント](https://www.php.net/manual/ja/function.date-parse.php)
- [DateTimeクラス](https://www.php.net/manual/ja/class.datetime.php)
- [Carbon](https://carbon.nesbot.com/)
- [Moment.js](https://momentjs.com/)