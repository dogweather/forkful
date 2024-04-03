---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:51.795911-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.276329-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u3092\u6271\u3046\
  \u3053\u3068\u306F\u3001\u30D7\u30EC\u30FC\u30F3\u30C6\u30AD\u30B9\u30C8\u3067\u8868\
  \u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u8868\u3059\u305F\u3081\u306E\u4EBA\u6C17\
  \u306E\u3042\u308B\u5F62\u5F0F\u3067\u3042\u308BCSV\u30D5\u30A1\u30A4\u30EB\u304B\
  \u3089\u306E\u8AAD\u307F\u53D6\u308A\u3068\u30C7\u30FC\u30BF\u306E\u66F8\u304D\u8FBC\
  \u307F\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u305D\u306E\u5358\u7D14\u3055\u3068\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\
  \u3084\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\u308F\u305F\u308B\
  \u5E83\u3044\u30B5\u30DD\u30FC\u30C8\u306E\u304A\u304B\u3052\u3067\u3001\u7570\u306A\
  \u308B\u30D7\u30ED\u30B0\u30E9\u30E0\u3001\u30B7\u30B9\u30C6\u30E0\u3001\u307E\u305F\
  \u306F\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u9593\u3067\u30C7\u30FC\u30BF\u3092\u7C21\
  \u5358\u306B\u4EA4\u63DB\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？

CSV（カンマ区切り値）を扱うことは、プレーンテキストで表形式のデータを表すための人気のある形式であるCSVファイルからの読み取りとデータの書き込みを含みます。プログラマーはその単純さとプラットフォームやプログラミング言語にわたる広いサポートのおかげで、異なるプログラム、システム、またはデータベース間でデータを簡単に交換するためにこれを行います。

## どうやって：

PHPはCSVファイルの取り扱いに対する組み込み関数を提供しており、サードパーティのライブラリを必要とせずにこれらのファイルから読み取り、書き込みを行うことを簡単にします。こちらは始めるための例です：

### CSVファイルの読み取り

`fopen()`と`fgetcsv()`を組み合わせてCSVファイルを開き、その内容を読み取ることができます：

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "行のフィールド数: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

このスクリプトは、各行のフィールド数とそのフィールド内容を表示します。

### CSVファイルへの書き込み

CSVファイルに書き込むために、書き込みモード（`w`）で`fopen()`と`fputcsv()`を使用します：

```php
<?php
$list = [
    ['ID', '名前', 'メール'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

このスクリプトは`users.csv`というファイルを作成し、ヘッダーと2行のデータを書き込みます。

### ライブラリを利用する：League\Csv

より高度なCSVの取り扱いには、`League\Csv`ライブラリが強力な機能セットを提供します。Composer（`composer require league/csv`）を介してインストールした後、より柔軟にCSVデータを読み書きするために使用できます。

#### League\Csvでの読み取り

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // 最初の行をヘッダーとして使いたい場合に設定

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

このスクリプトは、最初の行を列のヘッダーとして扱い、`data.csv`を読み取り、各行を連想配列として出力します。

#### League\Csvでの書き込み

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', '名前', 'メール']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "users_new.csvに正常に書き込みました。";
?>
```

これは、`users_new.csv`を作成し、ヘッダー行の後に2行のデータを書き込みます。
