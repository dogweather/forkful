---
title:                "CSVとの作業"
aliases:
- /ja/php/working-with-csv.md
date:                  2024-02-03T19:20:51.795911-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSVとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
