---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:23.521590-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306F\u3001\u30C7\u30D5\u30A9\u30EB\u30C8\
  \u3067\u306FYAML\u3092\u89E3\u6790\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\
  \u307F\u306Ecmdlet\u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u304C\u3001`powershell-yaml`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u6D3B\u7528\u3059\u308B\u304B\u3001`ConvertFrom-\u2026"
lastmod: '2024-03-13T22:44:42.465339-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306F\u3001\u30C7\u30D5\u30A9\u30EB\u30C8\u3067\u306FYAML\u3092\
  \u89E3\u6790\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u306Ecmdlet\u3092\
  \u6301\u3063\u3066\u3044\u307E\u305B\u3093\u304C\u3001`powershell-yaml`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u3092\u6D3B\u7528\u3059\u308B\u304B\u3001`ConvertFrom-Json`\u3092\
  `yq`\u306E\u3088\u3046\u306A\u30C4\u30FC\u30EB\u3068\u7D44\u307F\u5408\u308F\u305B\
  \u3066YAML\u3092PowerShell\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\
  \u3059\u308B\u3068\u3001YAML\u3068\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u52D5\u4F5C\
  \u3057\u307E\u3059."
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
