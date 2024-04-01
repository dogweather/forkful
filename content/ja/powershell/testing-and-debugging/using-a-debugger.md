---
date: 2024-01-26 04:09:25.132479-07:00
description: "\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3059\u308B\u3068\u3044\
  \u3046\u3053\u3068\u306F\u3001\u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\
  \u8A2D\u5B9A\u3057\u3001\u30B3\u30FC\u30C9\u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\
  \u3057\u3001\u5909\u6570\u3092\u30A6\u30A9\u30C3\u30C1\u3057\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u304C\u5B9F\u884C\u3055\u308C\u308B\u969B\u306E\u72B6\u614B\u3092\u691C\
  \u67FB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306F\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3068\u3063\u3066\u30B2\u30FC\u30E0\
  \u30C1\u30A7\u30F3\u30B8\u30E3\u30FC\u3067\u3042\u308A\u3001\u30D0\u30B0\u3092\u7279\
  \u5B9A\u3057\u3001\u79C1\u305F\u3061\u306E\u30B3\u30FC\u30C9\u304C\u5B9F\u969B\u306B\
  \u4F55\u3092\u3057\u3066\u3044\u308B\u306E\u304B\u3092\u7406\u89E3\u3059\u308B\u306E\
  \u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.443772-06:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3059\u308B\u3068\u3044\
  \u3046\u3053\u3068\u306F\u3001\u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\
  \u8A2D\u5B9A\u3057\u3001\u30B3\u30FC\u30C9\u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\
  \u3057\u3001\u5909\u6570\u3092\u30A6\u30A9\u30C3\u30C1\u3057\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u304C\u5B9F\u884C\u3055\u308C\u308B\u969B\u306E\u72B6\u614B\u3092\u691C\
  \u67FB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306F\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3068\u3063\u3066\u30B2\u30FC\u30E0\
  \u30C1\u30A7\u30F3\u30B8\u30E3\u30FC\u3067\u3042\u308A\u3001\u30D0\u30B0\u3092\u7279\
  \u5B9A\u3057\u3001\u79C1\u305F\u3061\u306E\u30B3\u30FC\u30C9\u304C\u5B9F\u969B\u306B\
  \u4F55\u3092\u3057\u3066\u3044\u308B\u306E\u304B\u3092\u7406\u89E3\u3059\u308B\u306E\
  \u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
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
