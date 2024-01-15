---
title:                "「CSVとの作業」"
html_title:           "PHP: 「CSVとの作業」"
simple_title:         "「CSVとの作業」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSVはコンマで区切られたデータを表す形式で、プログラム間でデータを交換するための便利な方法です。PHPでは、CSVファイルを扱うための様々な関数が用意されており、データ処理の機能を効率的に行うことができます。

## How To

```PHP
<?php
// CSVファイルを読み込む
$file = fopen("sample.csv", "r");

// データを配列に格納する
$data = [];
while (($row = fgetcsv($file)) !== false) {
    $data[] = $row;
}

// データを出力する
foreach ($data as $row) {
    echo implode(",", $row) . "\n";
}

// 新しい行を追加する
$add_row = ["New", "Data", "Entry"];
$data[] = $add_row;

// CSVファイルに新しい行を書き込む
$new_file = fopen("new_sample.csv", "w");
foreach ($data as $row) {
    fputcsv($new_file, $row);
}

// ファイルをクローズする
fclose($file);
fclose($new_file);
?>
```

上記の例では、まず```fopen()```関数を使用してCSVファイルを読み込みます。データを配列に格納するために```fgetcsv()```関数を使用し、組み込みの```\n```を使用して出力することができます。新しい行を追加するには、配列に追加した後に```fputcsv()```関数を使用して新しいCSVファイルに書き込みます。そして最後に、ファイルをクローズすることを忘れないようにしましょう。

## Deep Dive

CSVファイルを扱う際に注意する点としては、文字コードの違いやエスケープ文字の処理があります。文字コードが異なる場合、データを正しく読み取ることができず、エスケープ文字が含まれる場合はデータが壊れる可能性があります。そのため、ファイルを読み込む際には```fopen()```関数の第二引数で文字コードを指定することや、```fputcsv()```関数を使用する際にはエスケープ文字の設定を行う必要があります。

また、CSVファイルを扱う際にはデータの整形やフィルタリングも重要です。そのため、PHPでは```array_map()```や```array_filter()```といった便利な関数を使用することで、データの加工を簡単に行うことができます。

## See Also

- [PHP公式ドキュメント - CSV関数](https://www.php.net/manual/ja/ref.csv.php)
- [PHP: array_map - Manual](https://www.php.net/manual/ja/function.array-map.php)
- [PHP: array_filter - Manual](https://www.php.net/manual/ja/function.array-filter.php)