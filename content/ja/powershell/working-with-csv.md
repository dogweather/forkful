---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (CSVの取り扱いとその理由)
CSVはデータを保存する一般的なフォーマットです。プログラマーはデータ交換やテーブルデータの単純処理のために使います。

## How to: (方法)
### CSVを読む
```PowerShell
$data = Import-Csv -Path "data.csv"
$data
```
```
Name    Age
----    ---
Alice   23
Bob     34
```

### CSVを書く
```PowerShell
$data = @(
    [PSCustomObject]@{Name='Alice'; Age=23},
    [PSCustomObject]@{Name='Bob'; Age=34}
)
$data | Export-Csv -Path "output.csv" -NoTypeInformation
Get-Content "output.csv"
```
```
"Name","Age"
"Alice","23"
"Bob","34"
```

## Deep Dive (詳細な情報)
CSVはComma-Separated Valuesの略で、1972年にIBMが初めて使用しました。代わりにJSONやXMLが使えますが、CSVはそのシンプルさで広く使われます。PowerShellでは、`Import-Csv`と`Export-Csv`コマンドで簡単にCSVの読み書きができます。また、エンコードやデリミターなど、細かい設定も可能です。

## See Also (参照)
- [About Import-Csv - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [About Export-Csv - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv)
