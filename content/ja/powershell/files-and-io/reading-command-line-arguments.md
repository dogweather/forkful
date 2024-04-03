---
date: 2024-01-20 17:56:30.904808-07:00
description: "How to (\u65B9\u6CD5) PowerShell\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\
  \u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\u53D6\u308B\u306E\u306F\u7C21\u5358\u3067\
  \u3059\u3002`$args` \u5909\u6570\u3092\u4F7F\u3063\u3066\u30A2\u30AF\u30BB\u30B9\
  \u3057\u307E\u3057\u3087\u3046\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.458590-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\
  \u8AAD\u307F\u53D6\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002`$args` \u5909\
  \u6570\u3092\u4F7F\u3063\u3066\u30A2\u30AF\u30BB\u30B9\u3057\u307E\u3057\u3087\u3046\
  \u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
