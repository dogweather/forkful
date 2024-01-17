---
title:                "現在の日付の取得"
html_title:           "PHP: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何をするもの？ 
現在の日付を取得するとは、プログラマーがコードを使用して現在の日付を表示させることです。これは、アプリケーションやウェブサイトでよく使われる基本的な機能の一つです。プログラマーは、より正確な日付情報を表示させるために、様々な方法で現在の日付を取得します。

## やり方：
```PHP
echo date("Y-m-d"); //2021-09-15
```
PHPの```date()```関数を使用することで、現在の日付を簡単に取得することができます。この関数には、日付を表示するためのフォーマットを指定することができます。例えば、上記の例では、年月日の順番で日付を表示させています。

## 詳しく見る：
現在の日付を取得する方法は、プログラミング言語によって異なります。一般的には、システムのローカルタイムを使用して日付を取得することが多いですが、PHPでは独自の日付関数を提供しています。また、日付のフォーマットをカスタマイズすることもできます。例えば、```date("l, F jS Y"); //Wednesday, September 15th 2021```のように、曜日や月を表示させることもできます。

## 関連情報：
- [PHP公式ドキュメント：日付と時刻のフォーマット](https://www.php.net/manual/en/function.date.php)
- [PHP公式ドキュメント：日付と時刻の関数](https://www.php.net/manual/en/book.datetime.php)
- [W3Schools：PHPでの日付と時刻の扱い方](https://www.w3schools.com/php/php_date.asp)