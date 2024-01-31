---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:37:48.221884-07:00
simple_title:         "文字列から日付を解析する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付のパースとは、文字列から日付データを抽出するプロセスです。なぜこれが必要かというと、フォームの入力など、様々な形式の日付を統一された形式に変換し、データベースに保存したり、日付計算を行うためです。

## How to: (方法)
PHPでは`DateTime`クラスを使って日付をパースします。以下のコードを見てください。

```php
<?php
// 日付文字列のパース
$dateString = '2023-04-12 14:00:00';
$dateObject = new DateTime($dateString);

// 日付の出力
echo $dateObject->format('Y年m月d日 H:i:s');
```

出力:
```
2023年04月12日 14:00:00
```

曜日を取得するには？

```php
echo $dateObject->format('Y年m月d日 l');
```

出力:
```
2023年04月12日 Wednesday
```

## Deep Dive (詳細な情報)
初期のPHPでは、日付のパースには`strtotime()`と`date()`関数を使っていました。しかし、`DateTime`クラスが導入されてからは、オブジェクト指向のアプローチでより柔軟な日付時刻処理が可能になりました。例として、タイムゾーンのサポート、DateTimeImmutableクラスを用いた変更不可な日付時刻オブジェクトの作成などがあります。代替としてまだ`date()`関数を使うこともできますが、`DateTime`クラスが提供する機能の多さと扱いやすさから、今日では`DateTime`を使う方が一般的です。

たとえば、タイムゾーンを考慮したパースも簡単です。

```php
$dateObject = new DateTime($dateString, new DateTimeZone('Asia/Tokyo'));
```

これで、日本時間を正しく扱うことができます。

## See Also (関連情報)
- [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
- [PHP: DateTimeZone - Manual](https://www.php.net/manual/en/class.datetimezone.php)
- [PHP: Date/Time Functions - Manual](https://www.php.net/manual/en/book.datetime.php)
