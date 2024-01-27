---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSONはデータ交換のための軽量なフォーマットです。プログラマーはAPI通信、設定ファイル、その他多くの場面でJSONと作業をします。

## How to:
### JSONデータの読み込み
```PowerShell
# JSONファイルの読み取り
$json = Get-Content -Path 'data.json' | ConvertFrom-Json

# 結果の表示
$json
```

### JSONデータの生成
```PowerShell
# PowerShellオブジェクトの作成
$pso = [PSCustomObject]@{
  Name = 'Taro'
  Age = 30
  Email = 'taro@example.com'
}

# JSONに変換して表示
$pso | ConvertTo-Json
```

### JSONデータの変更と保存
```PowerShell
# 既存のJSONオブジェクトにプロパティ追加
$json | Add-Member -Type NoteProperty -Name 'PhoneNumber' -Value '123-456-7890'

# 変更したオブジェクトをJSONとして保存
$json | ConvertTo-Json | Set-Content -Path 'data_modified.json'
```

## Deep Dive
JSON (JavaScript Object Notation)は2000年代初頭にダグラス・クロックフォードによって考案されました。XMLの軽量な代替として普及。PowerShellでは`ConvertFrom-Json`と`ConvertTo-Json`コマンドレットで簡単に操作が可能。内部実装は.NETのJson.NETライブラリを利用。

## See Also
- [PowerShell 公式ドキュメント](https://docs.microsoft.com/ja-jp/powershell/)
- [Json.NETライブラリ](https://www.newtonsoft.com/json)
