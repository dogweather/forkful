---
title:                "デバッガーの使い方"
aliases:
- ja/powershell/using-a-debugger.md
date:                  2024-01-26T04:09:25.132479-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーを使用するということは、ブレークポイントを設定し、コードをステップ実行し、変数をウォッチし、プログラムが実行される際の状態を検査することを意味します。これはプログラマーにとってゲームチェンジャーであり、バグを特定し、私たちのコードが実際に何をしているのかを理解するのに役立ちます。

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
