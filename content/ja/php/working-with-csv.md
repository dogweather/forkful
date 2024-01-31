---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSV（Comma-Separated Values）は、データをコンマで区切ったテキスト形式です。PHPでCSVを扱うときは、データのインポート・エクスポートが簡単になり、表計算ソフトやデータベースとの相互運用が容易になります。

## How to: (方法)
```php
<?php
// CSVファイルの読み込み
$handle = fopen('sample.csv', 'r');
while (($data = fgetcsv($handle)) !== FALSE) {
    echo '行：' . implode(',', $data) . PHP_EOL;
}
fclose($handle);

// CSVファイルへの書き込み
$handle = fopen('output.csv', 'w');
fputcsv($handle, ['名前', 'メール', '年齢']);
fputcsv($handle, ['山田太郎', 'taro@example.com', '30']);
fclose($handle);
?>

// 出力例
行：名前,メール,年齢
行：山田太郎,taro@example.com,30
```

## Deep Dive (深まる情報)
CSV形式は1970年代から使われています。JSONやXMLなど他のデータ形式もありますが、CSVはそのシンプルさゆえに今でも使われています。PHPでは`fgetcsv`や`fputcsv`といった関数を使い、ローカルまたはリモートのCSVファイルを簡単に読み書きできます。注意するべき点は、異なるプラットフォーム間での改行コードの違いです。

## See Also (関連情報)
- PHP Manual on fgetcsv: https://www.php.net/manual/en/function.fgetcsv.php
- PHP Manual on fputcsv: https://www.php.net/manual/en/function.fputcsv.php
- Wikipedia on CSV: https://en.wikipedia.org/wiki/Comma-separated_values
- CSV handling with PHP by Stack Overflow: https://stackoverflow.com/questions/tagged/php+csv
