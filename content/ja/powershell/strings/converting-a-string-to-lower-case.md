---
date: 2024-01-20 17:39:02.138334-07:00
description: "How to: / \u65B9\u6CD5 PowerShell\u3067\u6587\u5B57\u5217\u3092\u5C0F\
  \u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u306B\u306F\u3001`.ToLower()` \u30E1\u30BD\
  \u30C3\u30C9\u3092\u4F7F\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.414959-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\u3067\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\
  \u3059\u308B\u306B\u306F\u3001`.ToLower()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3044\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## How to: / 方法
PowerShellで文字列を小文字に変換するには、`.ToLower()` メソッドを使います。以下に例を示します。

```PowerShell
# 文字列を定義する
$string = "Hello, World!"

# 文字列を小文字に変換する
$lowerCaseString = $string.ToLower()

# 結果を表示する
$lowerCaseString
```

これは次のような出力を生成します。

```
hello, world!
```

## Deep Dive / 詳細情報
文字列を小文字に変換する処理は、文字列の正規化の一部です。歴史的には、大文字と小文字が区別され始めたのは、印刷技術が発展した中世にさかのぼります。プログラミングにおいて、様々な文化や言語環境で文字列を扱う場合、.NET Frameworkの`ToLower()` メソッドは、カルチャに依存する方法と依存しない方法の両方を提供します。カルチャ非依存の変換には`.ToLowerInvariant()`が使われます。

ほかの方法としては、`[string]::ToLower()` スタティックメソッドがありますが、通常`.ToLower()` メソッドで十分です。注意点として、特定の文字はカルチャによって小文字変換の挙動が異なることがあるため、国際化されたアプリケーションでの使用では慎重に選択する必要があります。

## See Also / 関連情報
- `.NET` の文字列操作についての詳細: [Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.string.tolower)
- 文字列の大文字と小文字を区別しない比較に関する情報: [Microsoft Docs](https://docs.microsoft.com/dotnet/standard/base-types/best-practices-strings#comparing)
