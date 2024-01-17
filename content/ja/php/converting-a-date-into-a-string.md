---
title:                "「日付を文字列に変換する」"
html_title:           "PHP: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換するとは何か？それを行うプログラマーの理由は何か？

日付を文字列に変換することは、日付データを読みやすくするために行われます。プログラマーは、日付を文字列に変換することで、データを整形し、利用しやすくすることができます。

## 方法：

 ```PHP
<?php
$date = date_create('2021-07-15');
echo date_format($date, 'Y年m月d日');
// Output: 2021年07月15日
?>
```

日付を文字列に変換する最も単純な方法は、date_format関数を使用することです。この関数は、date_create関数で作成した日付オブジェクトと、出力形式を指定するフォーマット文字列を受け取ります。

 ```PHP
<?php
$date = new DateTime('2021-07-15');
echo $date->format('Y年m月d日');
// Output: 2021年07月15日
?>
```

また、DateTimeオブジェクトを作成し、formatメソッドを使用して日付を指定したフォーマットで文字列として出力することもできます。

## 深堀り：

日付を文字列に変換する方法は、PHPのバージョンによってやや異なります。古いバージョンのPHPでは、strtotime関数を使用して日付を文字列に変換するしかありませんでした。最新のPHPでは、DateTimeオブジェクトを使用することでより柔軟に日付を文字列に変換できるようになりました。

代替手段として、フォーマット文字列を使用しないstrftime関数を使用する方法もあります。この関数は、ロケールに応じた形式で日付を出力することができます。

日付を文字列に変換する際には、出力形式のフォーマット文字列を正しく指定することが重要です。例えば、'Y-m-d'というフォーマットは「年-月-日」の順になりますが、'd-m-Y'というフォーマットは「日-月-年」の順になります。詳細なフォーマットについては、PHP公式ドキュメントを参照してください。

## 関連リンク：

- PHP公式ドキュメント（日付のフォーマット方法）：https://www.php.net/manual/ja/datetime.format.php
- PHP公式ドキュメント（日付のフォーマット文字列）：https://www.php.net/manual/ja/datetime.formats.php
- strftime関数：https://www.php.net/manual/ja/function.strftime.php