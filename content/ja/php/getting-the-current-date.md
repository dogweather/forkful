---
title:    "PHP: 現在の日付を取得する"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ日付を取得するのか

現代のプログラミングでは、日付と時刻を取得することは非常に重要です。特定の日付や時間に処理を実行したり、現在の時刻を表示したりすることが必要になるケースは数多くあります。今回は、PHPプログラミングで日付を取得する方法について紹介します。

## 方法：日付を取得する基本的なコード

PHPでは、組み込み関数である`date()`を使用して日付を取得することができます。以下のコードを参考にしてください。

```PHP
<?php
$date = date('Y-m-d'); // YYYY-MM-DD形式の現在の日付を取得
echo $date; // "2021-09-15"のように表示される
```

上記のコードでは、`date()`関数に表示したい日付のフォーマットを指定する必要があります。`Y`は4桁の年、`m`は2桁の月、`d`は2桁の日を表しています。それぞれのフォーマットを組み合わせることで、さまざまな日付の表記が可能です。

## 深い情報：`date()`関数のパラメーター

`date()`関数では、取得したい日付のフォーマットを指定するだけでなく、より多くのパラメーターを利用することができます。例えば、以下のように`date()`のパラメーターを指定することで、週の最初の日付や12時間制の時刻などを取得することができます。

```PHP
<?php
$date = date('l, F jS', strtotime('next week')); // "September 20th"のように表示される
$time = date('h:i A'); // "10:30 AM"のように表示される
```

詳しいパラメーターの指定方法はPHPの公式ドキュメントを参照することをおすすめします。

## 参考リンク

- [PHP: date - Manual](https://www.php.net/manual/en/function.date.php)
- [PHP: strtotime - Manual](https://www.php.net/manual/en/function.strtotime.php)
- [Getting the Current Date and Time in PHP](https://www.w3schools.com/php/php_date.asp)
- [PHPで日付・時刻を取得する方法【date, time, strftime】](https://techacademy.jp/magazine/19110) (Japanese) 

## 参考

[PHPで日付を取得する方法（「date」関数）](https://programming-study.com/php/get-date/) (Japanese)