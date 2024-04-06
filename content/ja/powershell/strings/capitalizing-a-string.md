---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:13.144344-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306F\u591A\u6A5F\u80FD\u306A\u30C4\u30FC\
  \u30EB\u3067\u3042\u308A\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\u76F4\u63A5\
  \u7684\u306A\u65B9\u6CD5\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\
  \u3067\u5B9F\u73FE\u3067\u304D\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.237844-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
PowerShellは多機能なツールであり、サードパーティのライブラリを必要とせずに、直接的な方法で文字列を大文字化することができます。以下の方法で実現できます：

```powershell
# .Netの組み込みメソッド「ToTitleCase」をCultureInfoから使用
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
出力：
```
Hello world
```

注：この方法は各単語の最初の文字を大文字にします。文字列の最初の文字のみを大文字にして、残りをそのままにしたい場合は、以下のようにすることができます：

```powershell
# 文字列の最初の文字のみを大文字化
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
出力：
```
Hello world
```

PowerShellは、文字列の最初の文字のみを大文字にするためのシンプルな関数を直接には含んでいませんが、`Substring(0,1).ToUpper()`のような基本的な文字列操作メソッドと結合を組み合わせることで、希望する結果を簡単に実現できます。
