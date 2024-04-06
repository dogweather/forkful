---
date: 2024-01-20 17:53:03.081888-07:00
description: "How to: (\u3084\u308A\u65B9) PowerShell\u3067\u306F`Write-Host`\u3084\
  `Write-Debug`\u3092\u4F7F\u3063\u3066\u30C7\u30D0\u30C3\u30B0\u60C5\u5831\u3092\u8868\
  \u793A\u3067\u304D\u307E\u3059\u3002\u30B7\u30F3\u30D7\u30EB\u306A\u4F8B\u3092\u898B\
  \u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.261930-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) PowerShell\u3067\u306F`Write-Host`\u3084`Write-Debug`\u3092\
  \u4F7F\u3063\u3066\u30C7\u30D0\u30C3\u30B0\u60C5\u5831\u3092\u8868\u793A\u3067\u304D\
  \u307E\u3059\u3002\u30B7\u30F3\u30D7\u30EB\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002"
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
