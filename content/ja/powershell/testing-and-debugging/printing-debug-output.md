---
date: 2024-01-20 17:53:03.081888-07:00
description: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237\u306F\u30B3\u30FC\
  \u30C9\u306B\u554F\u984C\u304C\u306A\u3044\u304B\u78BA\u304B\u3081\u308B\u305F\u3081\
  \u306B\u4F7F\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\
  \u3092\u901A\u3057\u3066\u30B3\u30FC\u30C9\u306E\u52D5\u4F5C\u3092\u898B\u3066\u3001\
  \u554F\u984C\u3092\u7279\u5B9A\u3057\u3084\u3059\u304F\u306A\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.440465-06:00'
model: gpt-4-1106-preview
summary: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306E\u5370\u5237\u306F\u30B3\u30FC\
  \u30C9\u306B\u554F\u984C\u304C\u306A\u3044\u304B\u78BA\u304B\u3081\u308B\u305F\u3081\
  \u306B\u4F7F\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\
  \u3092\u901A\u3057\u3066\u30B3\u30FC\u30C9\u306E\u52D5\u4F5C\u3092\u898B\u3066\u3001\
  \u554F\u984C\u3092\u7279\u5B9A\u3057\u3084\u3059\u304F\u306A\u308A\u307E\u3059\u3002\
  ."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to: (やり方)
PowerShellでは`Write-Host`や`Write-Debug`を使ってデバッグ情報を表示できます。シンプルな例を見てみましょう。

```PowerShell
# 通常のテキスト出力
Write-Host "This is a regular message."

# デバッグ情報の出力
Write-Debug "This is a debug message."

# デバッグが有効の場合のみ表示
$DebugPreference = 'Continue'
Write-Debug "This debug message will be shown."
```

実行結果:

```
This is a regular message.
DEBUG: This is a debug message.
DEBUG: This debug message will be shown.
```

## Deep Dive (深掘り)
PowerShellでのデバッグ出力はデバッグが有効な環境でしか表示されません。`Write-Debug`を利用する前に`$DebugPreference`を設定することが重要です。`Write-Host`は常に表示されますが、デバッグ目的では推奨されません。歴史的に、デバッグは難解な問題を解決するためにさまざまな方法で行われてきましたが、PowerShellでは内蔵のコマンドレットで簡単に実現できます。他の代替方法にはログファイルへの書き込みやイベントログへの出力があります。

## See Also (参考情報)
- [about_Preference_Variables](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_preference_variables)
