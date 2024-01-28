---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:25:02.962257-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？

TOMLは、Tom's Obvious, Minimal Languageの略で、明快な意味論のために読みやすいデータシリアライゼーション形式です。プログラマーは、人間にも機械にも優しいバランスを提供するため、設定ファイルにこれを使用します。

## 方法：

PowerShellでは、TOMLを解析するためのネイティブなcmdletはありません。`toml-to-json`のようなツールを使ってTOMLをJSONに変換するか、モジュールを使用することが一般的です。以下は、架空のモジュール`PowerShellTOML`を使う方法です：

```PowerShell
# まず、モジュールをインストールします（架空のもので説明用）
Install-Module PowerShellTOML

# TOMLファイルをインポート
$config = Import-TomlConfig -Path './config.toml'

# 値にアクセス
Write-Output $config.database.server

# 'config.toml'のサンプルTOML内容：
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# サンプル出力：
# 192.168.1.1
```

## 深堀り

TOMLは、設定ファイル用のXMLやYAMLのより単純な代替として、GitHubの共同創業者であるTom Preston-Wernerによって作られました。その最初のバージョンは2013年に登場しました。TOMLはJSONと比較可能ですが、人間にとってよりフレンドリーに設計されており、人々によって維持される設定に適した選択肢です。代替には、YAML、JSON、XMLなどがあります。

実装の面では、TOMLのPowerShellモジュールは通常、C#のようなよりパフォーマンス指向の言語で書かれたTOMLライブラリのラッパーになります。PowerShellにはTOMLの組み込みサポートがないため、TOML形式を便利に扱うためにそのようなモジュールが必要です。

## 参照

- TOML標準：https://toml.io/en/
- `toml` PowerShellモジュールのGitHubリポジトリ（読む時点で存在する場合）：https://github.com/powershell/PowerShellTOML
- TOMLについての紹介：https://github.com/toml-lang/toml
- データシリアライゼーション形式の比較：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
