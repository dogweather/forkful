---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 日付を文字列に変換する- PHP

## 1. これは何で、なぜ必要か?

日付を文字列に変換するというのは、日付データを文字列形式に変更する処理のことで、これにより日付データをさまざまな方法で表現、操作することが可能になります。この操作はデータの出力形式を変更したり、特定の形式でデータベースに保存したりする場合に非常に役立ちます。

## 2. 実装方法:

以下のように、`date`関数を使用して日付を文字列に変換できます。

```PHP
<?php
$today = date("Y-m-d");
echo $today;
?>
```

この出力は現在の年、月、日を"-"(ハイフン)で区切った形式です。例えば"2021-12-31"のような結果になります。

## 3. 深掘り:

**歴史的背景**: PHPにおける`date`関数は非常に古い関数であり、初めてのPHPバージョンから存在しています。この関数は時間と日付を扱う上で、最も基本的な関数の一つです。

**代替手段**: `date`関数以外にも、以下のような関数・クラスでも日付を文字列に変換することが可能です。

- `strftime`関数
- DateTimeクラス

実装の詳細: `date`関数はシステムのデフォルトタイムゾーンを使用します。タイムゾーンを指定した日付を得るには、`date_default_timezone_set`関数を使用してタイムゾーンを事前に設定する必要があります。

## 4. 参考文献:

1. [PHP: date - Manual](https://www.php.net/manual/ja/function.date.php)
2. [PHP: strftime - Manual](https://www.php.net/manual/ja/function.strftime.php)
3. [PHP: DateTime - Manual](https://www.php.net/manual/ja/class.datetime.php)