---
title:                "現在の日付を取得する"
html_title:           "PHP: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

日付を取得する理由はさまざまです。たとえば、タイムスタンプを作成したり、日付を表示したりする必要がある場合があります。

## 方法

日付を取得するには、`date()`関数を使用します。この関数には、2つのパラメータがあります。最初のパラメータは、日付のフォーマットを示す文字列であり、2番目のパラメータは、オプションのタイムスタンプです。以下の例をご覧ください。

```PHP
今日の日付を取得するには、
今日の日付は <?php echo date("Y/m/d"); ?> です。
```

上記のコードを実行すると、以下の出力が得られます。

```
今日の日付は 2021/10/04 です。
```

日付だけでなく、現在の時刻や曜日も取得することができます。詳細なフォーマットについては、PHP公式ドキュメントをご覧ください。

## ディープダイブ

`date()`関数は、内部で現在のタイムスタンプを使用して日付を取得します。オプションのタイムスタンプを指定することで、異なる日付や時間を取得することもできます。また、PHPにはタイムゾーンを設定するための`date_default_timezone_set()`関数もあります。この関数を使用することで、現在のタイムゾーンに合った日付を取得することができます。

## 参考文献

- [PHP公式ドキュメント - date()関数](https://www.php.net/manual/en/function.date.php)
- [PHP公式ドキュメント - date_default_timezone_set()関数](https://www.php.net/manual/en/function.date-default-timezone-set.php)