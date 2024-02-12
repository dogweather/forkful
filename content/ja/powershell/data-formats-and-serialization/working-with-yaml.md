---
title:                "YAML を操作する"
aliases:
- ja/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:23.521590-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
