---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:35.790664-07:00
description: "PowerShell\u3068JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:42.466185-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3068JSON\uFF08JavaScript Object Notation\uFF09\u306E\u7D71\u5408\
  \u306F\u3001JSON\u30C7\u30FC\u30BF\u306E\u89E3\u6790\uFF08\u8AAD\u307F\u53D6\u308A\
  \uFF09\u3068\u751F\u6210\uFF08\u66F8\u304D\u8FBC\u307F\uFF09\u306B\u95A2\u3059\u308B\
  \u3082\u306E\u3067\u3059\u3002JSON\u306F\u3001\u8EFD\u91CF\u3067\u8A00\u8A9E\u306B\
  \u4F9D\u5B58\u3057\u306A\u3044\u6027\u8CEA\u306E\u305F\u3081\u3001\u30A6\u30A7\u30D6\
  \u4E0A\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u5171\u901A\u5F62\u5F0F\u3067\u3042\
  \u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30A6\u30A7\u30D6API\u3084\
  \u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3068\u306E\u5BFE\u8A71\u3001\u307E\u305F\u306F\
  \u7570\u306A\u308B\u8A00\u8A9E\u3084\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\
  \u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\
  \u3081\u306BJSON\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
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
