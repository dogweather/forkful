---
date: 2024-01-26 04:25:02.962257-07:00
description: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  \u660E\u5FEB\u306A\u610F\u5473\u8AD6\u306E\u305F\u3081\u306B\u8AAD\u307F\u3084\u3059\
  \u3044\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\
  \u5F62\u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\
  \u9593\u306B\u3082\u6A5F\u68B0\u306B\u3082\u512A\u3057\u3044\u30D0\u30E9\u30F3\u30B9\
  \u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.436090-07:00'
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\u660E\
  \u5FEB\u306A\u610F\u5473\u8AD6\u306E\u305F\u3081\u306B\u8AAD\u307F\u3084\u3059\u3044\
  \u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\
  \u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\
  \u306B\u3082\u6A5F\u68B0\u306B\u3082\u512A\u3057\u3044\u30D0\u30E9\u30F3\u30B9\u3092\
  \u63D0\u4F9B\u3059\u308B\u305F\u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
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
