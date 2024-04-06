---
date: 2024-01-26 04:09:25.132479-07:00
description: "\u4F7F\u3044\u65B9\uFF1A PowerShell\u3067\u306F\u3001\u7D44\u307F\u8FBC\
  \u307F\u306EPowerShell\u7D71\u5408\u30B9\u30AF\u30EA\u30D7\u30C8\u74B0\u5883\uFF08\
  ISE\uFF09\u307E\u305F\u306FVisual Studio Code\uFF08VS Code\uFF09\u306EPowerShell\u62E1\
  \u5F35\u6A5F\u80FD\u3092\u4F7F\u7528\u3057\u3066\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\
  \u30C7\u30D0\u30C3\u30B0\u304C\u53EF\u80FD\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  \u4E21\u65B9\u3067\u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\u4F7F\u7528\
  \u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.443772-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u306F\u3001\u7D44\u307F\u8FBC\u307F\u306EPowerShell\u7D71\
  \u5408\u30B9\u30AF\u30EA\u30D7\u30C8\u74B0\u5883\uFF08ISE\uFF09\u307E\u305F\u306F\
  Visual Studio Code\uFF08VS Code\uFF09\u306EPowerShell\u62E1\u5F35\u6A5F\u80FD\u3092\
  \u4F7F\u7528\u3057\u3066\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u30C7\u30D0\u30C3\u30B0\
  \u304C\u53EF\u80FD\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u4E21\u65B9\u3067\u30D6\
  \u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\
  \u3067\u3059\uFF1A\n"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 使い方：
PowerShellでは、組み込みのPowerShell統合スクリプト環境（ISE）またはVisual Studio Code（VS Code）のPowerShell拡張機能を使用してスクリプトのデバッグが可能です。以下は、両方でブレークポイントを使用する方法です：

### PowerShell ISE：
```PowerShell
# 特定の行にブレークポイントを設定
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# スクリプトを通常通り実行
.\MyScript.ps1

# スクリプトがブレークポイントに到達したら、変数を検査できます
$myVariable

# 実行を続行
Continue
```

### Visual Studio Code：
```PowerShell
# VS CodeでPowerShellスクリプトを開きます。
# 行番号の左をクリックしてブレークポイントを設定します。
# F5を押すか「デバッグの開始」をクリックしてデバッグを開始します。

# VS Codeはブレークポイントで実行を停止します。
# デバッグパネルを使用して変数をウォッチし、コールスタックを検査し、フローを制御します。
```

両環境でのデバッグでは、デバッグ中にステップイン（F11）、ステップオーバー（F10）、ステップアウト（Shift+F11）を可能にします。

## より深く
歴史的に、PowerShellでのデバッグは少々不器用で、変数の状態を出力するために多くの`Write-Host`行を必要としたり、古典的な試行錯誤法を必要としました。しかし、PowerShell ISEの登場、そして最近では、豊富なデバッグ機能を備えたVS Codeによって、PowerShellのデバッグは完全なプログラミング言語でのそれとほぼ同じくらい直感的になりました。

PowerShellのネイティブデバッグツールの代替には、PowerGUIのようなサードパーティのツールや、PowerShellプラグインを備えたVisual Studioのような強力なIDEの使用が含まれます。

デバッガーを実装する際には、ドットソースされたスクリプトやモジュールを扱う場合と同様に、スクリプトのスコープを考慮することが重要です。ブレークポイントは条件ベース、変数変更ベース、または行ベースであり、デバッグセッション中に精密な制御を可能にします。

さらに、PowerShell Core（クロスプラットフォームのPowerShell）への移行に伴い、デバッグは大きくVS Codeの手に移り、異なるプラットフォーム間で一貫した経験を提供します。

## 参照
PowerShellでのデバッグについての詳細は：
- [about_Debuggers](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_Debuggers)
