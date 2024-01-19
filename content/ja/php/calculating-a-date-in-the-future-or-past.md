---
title:                "未来または過去の日付の計算"
html_title:           "PHP: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ?
日付の計算は特定の日付から将来または過去の日付を割り出すことです。プログラマーは予定納期の計算、イベントのスケジューリングなど、多岐にわたるタスクを実行するためにこれを行います。

## 使い方:
以下は基本的なコード例とその出力です。現在日から30日後の日付を計算します。

```PHP
<?php
$current_date = date('Y-m-d'); // 現在日
$future_date = date('Y-m-d', strtotime("+30 days", strtotime($current_date)));
echo $future_date;
?>
```
出力:
```PHP
2022-04-20
```
同様に、過去の日付を計算するにはマイナス記号("-")を使用します。

```PHP
<?php
$current_date = date('Y-m-d'); // 現在日
$past_date = date('Y-m-d', strtotime("-20 days", strtotime($current_date)));
echo $past_date;
?>
```
出力:
```PHP
2022-03-11
```
## ディープダイブ:
歴史的な文脈: PHPの `strtotime` 関数は強力なUNIXタイムスタンプ変換ツールです。古くから存在していますが、現代のPHPでは日付計算にもっとも一般的に使われます。

代替案: `strtotime`に代わることが可能なのは `DateTime` オブジェクトとその `modify` メソッドです。 

```PHP
<?php
$date = new DateTime(); 
$date->modify('+1 month'); 
echo $date->format('Y-m-d');
?>
```

実装の詳細: `strtotime` 関数は、与えられた文字列表現をUNIXタイムスタンプ（1970年1月1日からの秒数）に変換します。これはさまざまな日付操作を非常に簡単にします。

## さらなる情報源:
- PHP公式ドキュメントの `strtotime`: https://www.php.net/manual/ja/function.strtotime.php
- PHP公式ドキュメントの `DateTime`: https://www.php.net/manual/ja/class.datetime.php
- PHPで日付を操作するいくつかの方法: https://www.php.net/manual/ja/datetime.modify.php.