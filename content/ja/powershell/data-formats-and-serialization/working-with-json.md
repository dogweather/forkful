---
title:                "JSONを活用する"
aliases:
- /ja/powershell/working-with-json/
date:                  2024-02-03T19:23:35.790664-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

PowerShellとJSON（JavaScript Object Notation）の統合は、JSONデータの解析（読み取り）と生成（書き込み）に関するものです。JSONは、軽量で言語に依存しない性質のため、ウェブ上のデータ交換の共通形式であり、プログラマーはウェブAPIや設定ファイルとの対話、または異なる言語やプラットフォーム間のデータ交換を容易にするためにJSONを使用します。

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
