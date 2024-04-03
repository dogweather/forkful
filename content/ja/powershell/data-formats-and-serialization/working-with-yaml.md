---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:23.521590-07:00
description: "YAML\u3001\u307E\u305F\u306FYAML Ain't Markup\u2026"
lastmod: '2024-03-13T22:44:42.465339-06:00'
model: gpt-4-0125-preview
summary: "YAML\u3001\u307E\u305F\u306FYAML Ain't Markup Language\u306F\u3001\u4EBA\
  \u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u8A00\u8A9E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3084\u8A00\u8A9E\u9593\
  \u306E\u30C7\u30FC\u30BF\u4F1D\u9001\u306B\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\
  \u3002\u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\u8AAD\u307F\u3084\u3059\u3055\
  \u304B\u3089\u3001\u74B0\u5883\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3001\u307E\u305F\u306F\u30B5\u30FC\u30D3\u30B9\u306E\u30BB\u30C3\u30C8\u30A2\u30C3\
  \u30D7\u306B\u95A2\u308F\u308B\u30BF\u30B9\u30AF\u3067\u7279\u306B\u4EBA\u6C17\u304C\
  \u3042\u308A\u3001\u8A2D\u5B9A\u306F\u91CD\u8981\u3067\u3042\u308A\u3001\u7C21\u5358\
  \u306B\u7406\u89E3\u3057\u3066\u7DE8\u96C6\u3067\u304D\u308B\u3079\u304D\u3067\u3059\
  \u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法：
PowerShellは、デフォルトではYAMLを解析するための組み込みのcmdletを持っていませんが、`powershell-yaml`モジュールを活用するか、`ConvertFrom-Json`を`yq`のようなツールと組み合わせてYAMLをPowerShellオブジェクトに変換すると、YAMLとシームレスに動作します。

### `powershell-yaml`モジュールの使用：
まず、モジュールをインストールします：
```PowerShell
Install-Module -Name powershell-yaml
```

YAMLファイルを読み取るには：
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

PowerShellオブジェクトをYAMLファイルに書き込むには：
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

`output.yml`サンプル：
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### `yq`と`ConvertFrom-Json`によるYAMLの解析：
別のアプローチは、`yq`を使用することです。`yq`は軽量でポータブルなコマンドラインYAMLプロセッサーで、YAMLをJSONに変換でき、PowerShellがネイティブに解析できます。

まず、システムに`yq`がインストールされていることを確認してください。
その後、実行します：
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

この方法は、クロスプラットフォーム環境で作業するユーザーや、PowerShell内でJSONを好んで使用するユーザーに特に便利です。
