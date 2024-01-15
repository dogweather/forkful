---
title:                "「二つの日付を比較する」"
html_title:           "PHP: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

なぜ日付を比較するのか？
日付を比較することで、二つの日付の間の差を計算したり、特定の期間内にあるかどうかを判定したりすることができます。これは特に日付を取り扱うアプリケーションやシステムを開発する際に重要な機能です。

## How To

日付を比較するには、PHPの組み込み関数である`strtotime()`と`date_diff()`を使用します。

### 例：二つの日付の差を計算する

```PHP
$date1 = strtotime("2020-01-01");
$date2 = strtotime("2020-02-01");

$diff = date_diff($date1, $date2);

echo $diff->format("%R%a days"); // Output: +31 days
```

上記のコードでは、`strtotime()`で日付をUnixタイムスタンプに変換し、`date_diff()`で二つの日付の差を計算しています。`$diff->format("%R%a days")`の部分では、日付の差を日単位で表示するようにフォーマットしています。

### 例：特定の期間内にあるかどうかを判定する

```PHP
$start_date = strtotime("2019-01-01");
$end_date = strtotime("2019-12-31");
$check_date = strtotime("2019-04-15");

if ($check_date >= $start_date && $check_date <= $end_date) {
  echo "This date is within the specified range."; // Output: This date is within the specified range.
} else {
  echo "This date is not within the specified range.";
}
```

上記のコードでは、`strtotime()`で日付をUnixタイムスタンプに変換し、`$check_date`が`$start_date`と`$end_date`の間にあるかどうかを`if`文で判定しています。

## Deep Dive

日付を比較する際には、いくつかの注意点に気を付ける必要があります。

- `strtotime()`は指定された日付をUnixタイムスタンプに変換するが、入力に応じて自動的に適切なタイムゾーンを選択しないため、環境によっては意図しない結果が得られる可能性があります。
- `date_diff()`は二つの日付を比較する際に、より大きい日付を基準にして差を計算します。つまり、`date1 > date2`の場合は`date1 - date2`となり、`date1 < date2`の場合は`date2 - date1`となります。

## See Also

「PHPにおける日付処理の基礎」(https://www.php.net/manual/ja/datetime.formats.date.php)
「Unixタイムスタンプとは？」(https://ja.wikipedia.org/wiki/Unix%E3%82%BF%E3%82%A4%E3%83%A0%E3%82%B9%E3%82%BF%E3%83%B3%E3%83%97)