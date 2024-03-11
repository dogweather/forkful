---
date: 2024-01-20 17:56:30.904808-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\
  \u53D6\u308A\u3068\u306F\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u60C5\u5831\u3092\
  \u6E21\u3059\u65B9\u6CD5\u306E\u4E00\u3064\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u3044\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u306E\u6319\u52D5\u3092\u67D4\u8EDF\u306B\u5236\u5FA1\u3057\u305F\u308A\u3001\u30E6\
  \u30FC\u30B6\u30FC\u306E\u5165\u529B\u3092\u53D7\u3051\u53D6\u3063\u305F\u308A\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:16.008285-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\
  \u53D6\u308A\u3068\u306F\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u306B\u60C5\u5831\u3092\
  \u6E21\u3059\u65B9\u6CD5\u306E\u4E00\u3064\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u3044\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u306E\u6319\u52D5\u3092\u67D4\u8EDF\u306B\u5236\u5FA1\u3057\u305F\u308A\u3001\u30E6\
  \u30FC\u30B6\u30FC\u306E\u5165\u529B\u3092\u53D7\u3051\u53D6\u3063\u305F\u308A\u3057\
  \u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
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
