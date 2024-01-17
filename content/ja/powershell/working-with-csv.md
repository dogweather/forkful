---
title:                "csvファイルの操作"
html_title:           "PowerShell: csvファイルの操作"
simple_title:         "csvファイルの操作"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## 何 & なぜ
CSVとは、Comma-Separated Valuesの略で、コンマで区切られたデータを表すファイル形式です。プログラマーがCSVを使う理由の一つは、データの整形や処理を容易にするためです。

## 方法：
```PowerShell
# CSVファイルの読み込み
$csv = Import-Csv -Path C:\data.csv

# CSVファイルにデータを書き込む
$data = "Name, Email, Phone
John, john@example.com, 123456789
Jane, jane@example.com, 987654321"
$data | Out-File -FilePath C:\new_data.csv
```

## 深堀り：
CSVは、データの交換や共有に幅広く利用されてきた古典的なフォーマットです。代わりにJSONやXMLといったフォーマットがあるものの、今でも多くのシステムでCSVが用いられています。PowerShell以外にも、PythonやJavaなどの言語でもCSVを処理することができます。

## 関連情報：
- [PowerShellでCSVを処理する方法](https://zetawiki.com/wiki/PowerShell%E3%81%A7CSV%E3%82%92%E5%87%A6%E7%90%86%E3%81%99%E3%82%8B)
- [CSVとは？その基本を学ぼう](https://techacademy.jp/magazine/16620)
- [CSVのメリットとデメリット](https://www.sejuku.net/blog/10836)