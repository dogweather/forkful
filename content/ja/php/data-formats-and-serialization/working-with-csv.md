---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:51.795911-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A PHP\u306FCSV\u30D5\u30A1\u30A4\u30EB\
  \u306E\u53D6\u308A\u6271\u3044\u306B\u5BFE\u3059\u308B\u7D44\u307F\u8FBC\u307F\u95A2\
  \u6570\u3092\u63D0\u4F9B\u3057\u3066\u304A\u308A\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\
  \u306B\u3053\u308C\u3089\u306E\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u8AAD\u307F\u53D6\
  \u308A\u3001\u66F8\u304D\u8FBC\u307F\u3092\u884C\u3046\u3053\u3068\u3092\u7C21\u5358\
  \u306B\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u306F\u59CB\u3081\u308B\u305F\u3081\
  \u306E\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.128328-06:00'
model: gpt-4-0125-preview
summary: ''
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
