---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.790664-07:00
description: "\u65B9\u6CD5: #."
lastmod: '2024-03-13T22:44:42.466185-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 方法:


### JSONの解析
PowerShellでJSONを読み込む、または解析するには、`ConvertFrom-Json` コマンドレットを使用できます。JSON文字列が与えられると、このコマンドレットはそれをPowerShellオブジェクトに変換します。

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

サンプル出力:

```
John Doe
```

この例は、単純なJSON文字列を解析して、結果のオブジェクトのプロパティにアクセスする方法を示しています。

### JSONの生成
PowerShellオブジェクトからJSONを生成するには、`ConvertTo-Json` コマンドレットを使用できます。これは、データをウェブサービスに送信する準備をしたり、設定ファイルに保存する場合に便利です。

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

サンプル出力:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

このコードスニペットは、PowerShellオブジェクトを作成し、それをJSON文字列に変換します。
