---
title:                "コマンドライン引数の読み取り"
aliases:
- /ja/powershell/reading-command-line-arguments/
date:                  2024-01-20T17:56:30.904808-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数の読み取りとは、スクリプトに情報を渡す方法の一つです。プログラマーはこれを使い、スクリプトの挙動を柔軟に制御したり、ユーザーの入力を受け取ったりします。

## How to (方法)
PowerShellでコマンドライン引数を読み取るのは簡単です。`$args` 変数を使ってアクセスしましょう。例を見てみましょう。

```PowerShell
# スクリプト名: greet.ps1

param(
  [String]$name,
  [String]$greeting = "こんにちは"
)

echo "$greeting, $name!"

# 別の方法としては、$argsを使用することができます。
# echo "こんにちは, $($args[0])!"
```

コマンドラインから実行:

```PowerShell
PS > .\greet.ps1 -name "世界"
こんにちは, 世界!
```

出力は次のとおりです:

```
こんにちは, 世界!
```

## Deep Dive (掘り下げ)
コマンドライン引数を読む方法は PowerShell の初期バージョンから存在しています。`$args`は自動的に全ての引数を配列として保持します。`param` ブロックを使うことで、より明示的にパラメータを定義することができます。

他にも方法はあります。例えば、`[CmdletBinding()]` 属性を使用して高度な関数（advanced functions）を作成することができます。これは、より詳細なパラメーター検証やデフォルト値の設定など、より複雑なスクリプトで役立つ機能を提供します。

## See Also (関連情報)
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [about Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
- [about Functions Advanced Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters)
