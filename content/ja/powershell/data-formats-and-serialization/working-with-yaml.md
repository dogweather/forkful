---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:23.521590-07:00
description: "YAML\u3001\u307E\u305F\u306FYAML Ain't Markup\u2026"
lastmod: '2024-03-11T00:14:16.015168-06:00'
model: gpt-4-0125-preview
summary: "YAML\u3001\u307E\u305F\u306FYAML Ain't Markup\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
YAML、またはYAML Ain't Markup Languageは、人が読みやすいデータシリアライゼーション言語です。プログラマーは、設定ファイルや言語間のデータ伝送によく使用します。そのシンプルさと読みやすさから、環境、アプリケーション、またはサービスのセットアップに関わるタスクで特に人気があり、設定は重要であり、簡単に理解して編集できるべきです。

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
