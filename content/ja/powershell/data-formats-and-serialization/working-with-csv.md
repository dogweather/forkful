---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:55.634050-07:00
description: "\u65B9\u6CD5\uFF1A CSV\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u8AAD\u307F\
  \u53D6\u308B\u306B\u306F\u3001`Import-Csv` \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\
  \u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u306E\u30B3\u30DE\u30F3\u30C9\u30EC\
  \u30C3\u30C8\u306F\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\u308A\u3001\u5404\
  \u884C\u306B\u5BFE\u3057\u3066\u30AB\u30B9\u30BF\u30E0\u306EPowerShell\u30AA\u30D6\
  \u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:41.975951-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A CSV\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u8AAD\u307F\u53D6\
  \u308B\u306B\u306F\u3001`Import-Csv` \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u306E\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\
  \u30C8\u306F\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\u308A\u3001\u5404\u884C\
  \u306B\u5BFE\u3057\u3066\u30AB\u30B9\u30BF\u30E0\u306EPowerShell\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3057\u307E\u3059\u3002"
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
