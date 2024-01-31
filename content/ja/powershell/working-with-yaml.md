---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
(何となぜ？)

YAMLは設定やデータ交換によく使います。読みやすく、シンプルな構造が特徴です。PowerShellでYAMLを操作する理由は、自動化スクリプトや設定ファイルの管理が簡単になるからです。

## How to:
(やり方)

PowerShellでYAMLを扱うには、`powershell-yaml`モジュールが便利です。以下のコマンドでインストールし、YAMLファイルを読み込んでみましょう。

```PowerShell
# powershell-yaml モジュールのインストール
Install-Module -Name powershell-yaml

# YAML ファイルの読み込み
$yamlContent = Get-Content -Path 'example.yaml' | ConvertFrom-Yaml

# 内容の表示
$yamlContent
```

`example.yaml` の内容を変更する例を示します。

```PowerShell
# 新しいデータの追加
$yamlContent.newKey = 'newValue'

# 変更をYAMLファイルに書き込む
$yamlContent | ConvertTo-Yaml | Set-Content -Path 'example.yaml'
```

実行結果は変更された `example.yaml` ファイルの内容になります。

## Deep Dive
(掘り下げ)

YAMLは「YAML Ain't Markup Language」（元々「Yet Another Markup Language」）を意味し、2001年に登場しました。JSONやXMLに代わる選択肢であり、構造が直感的でわかりやすいです。PowerShellでは、`powershell-yaml` モジュール以外にも、.NETライブラリを使ったり、外部のパーサを呼びだしたりする方法がありますが、`powershell-yaml`が最も簡潔です。

## See Also
(関連情報)

- [powershell-yaml GitHub page](https://github.com/cloudbase/powershell-yaml) - `powershell-yaml`の公式ドキュメントとソースコード
- [YAML specification](https://yaml.org/spec/) - YAMLの仕様に関する詳細情報
- [PowerShell Gallery](https://www.powershellgallery.com/packages/powershell-yaml) - `powershell-yaml`モジュールを含む他のPowerShellリソース
