---
title:                "デバッグ出力を表示する"
aliases:
- /ja/powershell/printing-debug-output/
date:                  2024-01-20T17:53:03.081888-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力の印刷はコードに問題がないか確かめるために使います。プログラマはこれを通してコードの動作を見て、問題を特定しやすくなります。

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
