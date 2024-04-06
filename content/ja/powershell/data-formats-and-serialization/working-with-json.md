---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.790664-07:00
description: "\u65B9\u6CD5: PowerShell\u3067JSON\u3092\u8AAD\u307F\u8FBC\u3080\u3001\
  \u307E\u305F\u306F\u89E3\u6790\u3059\u308B\u306B\u306F\u3001`ConvertFrom-Json` \u30B3\
  \u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\
  JSON\u6587\u5B57\u5217\u304C\u4E0E\u3048\u3089\u308C\u308B\u3068\u3001\u3053\u306E\
  \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u306F\u305D\u308C\u3092PowerShell\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.466185-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067JSON\u3092\u8AAD\u307F\u8FBC\u3080\u3001\u307E\u305F\u306F\
  \u89E3\u6790\u3059\u308B\u306B\u306F\u3001`ConvertFrom-Json` \u30B3\u30DE\u30F3\u30C9\
  \u30EC\u30C3\u30C8\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002JSON\u6587\u5B57\
  \u5217\u304C\u4E0E\u3048\u3089\u308C\u308B\u3068\u3001\u3053\u306E\u30B3\u30DE\u30F3\
  \u30C9\u30EC\u30C3\u30C8\u306F\u305D\u308C\u3092PowerShell\u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u306B\u5909\u63DB\u3057\u307E\u3059."
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
