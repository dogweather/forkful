---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:55.634050-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.467031-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\
  \u30EB\u3092\u64CD\u4F5C\u3059\u308B\u4F5C\u696D\u306F\u3001\u69CB\u9020\u5316\u3055\
  \u308C\u305F\u8868\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u7BA1\u7406\u3057\u3001\
  \u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u306E\u64CD\u4F5C\
  \u3092\u884C\u3044\u3001\u30C7\u30FC\u30BF\u5206\u6790\u3001\u30EC\u30DD\u30FC\u30C6\
  \u30A3\u30F3\u30B0\u3001\u3042\u308B\u3044\u306F\u30A6\u30A7\u30D6\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u5F37\u5316\u3059\u308B\u305F\u3081\u306A\u3069\
  \u3001\u69D8\u3005\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u30C7\
  \u30FC\u30BF\u3092\u52B9\u7387\u7684\u306B\u30A4\u30F3\u30DD\u30FC\u30C8\u3001\u30A8\
  \u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u307E\u305F\u306F\u64CD\u4F5C\u3057\u307E\u3059\
  \u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
