---
date: 2024-01-26 04:25:02.962257-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u3067\u306F\u3001TOML\u3092\u89E3\u6790\
  \u3059\u308B\u305F\u3081\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u306Acmdlet\u306F\u3042\
  \u308A\u307E\u305B\u3093\u3002`toml-to-json`\u306E\u3088\u3046\u306A\u30C4\u30FC\
  \u30EB\u3092\u4F7F\u3063\u3066TOML\u3092JSON\u306B\u5909\u63DB\u3059\u308B\u304B\
  \u3001\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\
  \u4E00\u822C\u7684\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u67B6\u7A7A\u306E\u30E2\
  \u30B8\u30E5\u30FC\u30EB`PowerShellTOML`\u3092\u4F7F\u3046\u65B9\u6CD5\u3067\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:38:41.977175-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A PowerShell\u3067\u306F\u3001TOML\u3092\u89E3\u6790\u3059\
  \u308B\u305F\u3081\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u306Acmdlet\u306F\u3042\u308A\
  \u307E\u305B\u3093\u3002`toml-to-json`\u306E\u3088\u3046\u306A\u30C4\u30FC\u30EB\
  \u3092\u4F7F\u3063\u3066TOML\u3092JSON\u306B\u5909\u63DB\u3059\u308B\u304B\u3001\
  \u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u4E00\
  \u822C\u7684\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u67B6\u7A7A\u306E\u30E2\u30B8\
  \u30E5\u30FC\u30EB`PowerShellTOML`\u3092\u4F7F\u3046\u65B9\u6CD5\u3067\u3059\uFF1A\
  ."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
