---
title:                "日付を文字列に変換する"
html_title:           "PHP: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何故？
日付を文字列に変換する必要性は、データベースから取得した日付を人間が読みやすい形式に表示するためです。また、国や地域ごとの標準形式に合わせる必要がある場合もあります。

## 方法
```PHP
$date = new DateTime('2021-12-31');
echo $date->format('Y/m/d'); // Output: 2021/12/31
```

日付を文字列に変換するには、DateTimeクラスを使用し、formatメソッドで表示したい形式を指定します。この例では、"Y/m/d"という形式で表示するよう指定しています。

```PHP
$date = new DateTime('2021-12-31');
setlocale(LC_ALL, 'ja_JP');
echo strftime('%Y年%m月%d日', $date->getTimestamp()); // Output: 2021年12月31日
```

DateTimeクラスを使用しない場合は、setlocale関数を使用して日本語のロケールを設定し、strftime関数を使用して日付を指定した形式で表示することもできます。

## 深堀り
日付を変換する際に注意すべきことは、データベースから取得した日付が正しく取得できているかどうかを確認することです。また、表示したい形式に合わせるために、DateTimeオブジェクトを作成する際にタイムゾーンを指定することも大切です。

## See Also
- DateTimeクラス: https://www.php.net/manual/ja/class.datetime.php
- setlocale関数: https://www.php.net/manual/ja/function.setlocale.php
- strftime関数: https://www.php.net/manual/ja/function.strftime.php