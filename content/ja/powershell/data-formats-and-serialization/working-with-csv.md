---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:55.634050-07:00
description: "\u2026"
lastmod: 2024-02-19 22:05:01.585938
model: gpt-4-0125-preview
summary: "\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ？

CSV（カンマ区切り値）ファイルを操作する作業は、構造化された表形式のデータを管理し、操作するための一般的なタスクです。プログラマーはこの操作を行い、データ分析、レポーティング、あるいはウェブアプリケーションを強化するためなど、様々なアプリケーションでデータを効率的にインポート、エクスポート、または操作します。

## 方法：

### CSVファイルの読み込み

CSVファイルから読み取るには、`Import-Csv` コマンドレットを使用します。このコマンドレットはファイルを読み取り、各行に対してカスタムのPowerShellオブジェクトに変換します。

```powershell
# CSVファイルをインポート
$data = Import-Csv -Path "C:\Data\users.csv"
# 内容を表示
$data
```

**サンプル出力：**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### CSVファイルへの書き込み

逆に、CSVファイルにデータを書き込むには、`Export-Csv`コマンドレットを使用します。このコマンドレットは入力オブジェクトを取り、それをCSV形式に変換します。

```powershell
# エクスポートするオブジェクトの作成
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# CSVファイルにエクスポート
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

実行後、「new_users.csv」という名前のファイルが提供されたデータで作成されます。

### CSVコンテンツのフィルタリングと操作

CSVファイルからのデータをフィルタリングまたは操作するには、PowerShellのオブジェクト操作機能を使用します。たとえば、ある特定の年齢以上のユーザーおよび特定の都市からのユーザーのみを選択する場合：

```powershell
# データのインポートとフィルタリング
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# フィルタリングされたデータを表示
$filteredData
```

**サンプル出力：**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### サードパーティのライブラリを使用する

一般的なタスクにはPowerShellのネイティブなコマンドレットが通常十分な場合が多いですが、より複雑な操作にはサードパーティのライブラリやツールが役立つことがあります。ただし、読み取り、書き込み、フィルタリング、またはソートなど、標準的なCSV操作に関しては、`Import-Csv` や `Export-Csv` などのPowerShellの組み込みコマンドレットが通常、追加のライブラリなしで堅牢な機能を提供します。
