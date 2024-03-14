---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:13.144344-07:00
description: "PowerShell\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\
  \u308B\u3068\u306F\u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u6700\
  \u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\u3057\u3001\u6B8B\
  \u308A\u306E\u6587\u5B57\u5217\u3092\u5909\u66F4\u3057\u306A\u3044\u307E\u307E\u306B\
  \u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\
  \u30FC\u30B9\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u8868\u793A\u306E\u6E96\u5099\u3084\
  \u3001\u751F\u6210\u3055\u308C\u305F\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u6587\
  \u6CD5\u898F\u5247\u306B\u5F93\u3046\u305F\u3081\u306A\u3069\u3001\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u76EE\u7684\u3067\u3053\u306E\u30BF\u30B9\u30AF\u3092\u3088\u304F\
  \u5B9F\u884C\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.409356-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\
  \u3068\u306F\u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u6700\u521D\
  \u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\u3057\u3001\u6B8B\u308A\
  \u306E\u6587\u5B57\u5217\u3092\u5909\u66F4\u3057\u306A\u3044\u307E\u307E\u306B\u3059\
  \u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\
  \u30B9\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u8868\u793A\u306E\u6E96\u5099\u3084\u3001\
  \u751F\u6210\u3055\u308C\u305F\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u6587\u6CD5\
  \u898F\u5247\u306B\u5F93\u3046\u305F\u3081\u306A\u3069\u3001\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u76EE\u7684\u3067\u3053\u306E\u30BF\u30B9\u30AF\u3092\u3088\u304F\u5B9F\
  \u884C\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
PowerShellで文字列を大文字化するとは、与えられた文字列の最初の文字を大文字に変換し、残りの文字列を変更しないままにすることを指します。プログラマーは、ユーザーインターフェースでのテキスト表示の準備や、生成されたドキュメントの文法規則に従うためなど、フォーマット目的でこのタスクをよく実行します。

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
