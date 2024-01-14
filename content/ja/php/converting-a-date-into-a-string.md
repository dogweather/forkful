---
title:                "PHP: 日付を文字列に変換する"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することについて興味がある方へ、調べてみました。

## 方法

「date_format」関数を使って、日付を文字列に変換する方法をご紹介します。

```PHP
<?php
$date = date_create('2021-10-20');
echo date_format($date, 'Y年m月d日'); //出力結果：2021年10月20日
echo date_format($date, 'M d, Y'); //出力結果：Oct 20, 2021
?>
```

## ディープダイブ

日付を文字列に変換する際、よく使われる「date」関数や「strftime」関数の違いや使い方について掘り下げています。また、文字列のフォーマットをカスタマイズする方法や注意点についても詳しく解説しています。

## 参考リンク

- [PHP: date_format 関数](https://www.php.net/manual/ja/function.date-format.php)
- [PHP: date 関数](https://www.php.net/manual/ja/function.date.php)
- [PHP: strftime 関数](https://www.php.net/manual/ja/function.strftime.php)
- [Date and Time Formats - PHP Manual](https://www.php.net/manual/en/datetime.format.php)