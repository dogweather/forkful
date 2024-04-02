---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:53.264094-07:00
description: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u691C\u7D22\u30D1\u30BF\
  \u30FC\u30F3\u3092\u5F62\u6210\u3059\u308B\u6587\u5B57\u306E\u5217\u3067\u3042\u308A\
  \u3001\u4E3B\u306B\u6587\u5B57\u5217\u306E\u691C\u7D22\u3084\u64CD\u4F5C\u306B\u4F7F\
  \u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306E\u51E6\u7406\u306B\u304A\u3044\u3066\
  \u305D\u306E\u52B9\u7387\u6027\u3068\u67D4\u8EDF\u6027\u304B\u3089\u3001PowerShell\u3067\
  \u306E\u30C7\u30FC\u30BF\u691C\u8A3C\u3001\u89E3\u6790\u3001\u5909\u63DB\u306A\u3069\
  \u306E\u30BF\u30B9\u30AF\u306Bregex\u3092\u6D3B\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.420154-06:00'
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u691C\u7D22\u30D1\u30BF\u30FC\
  \u30F3\u3092\u5F62\u6210\u3059\u308B\u6587\u5B57\u306E\u5217\u3067\u3042\u308A\u3001\
  \u4E3B\u306B\u6587\u5B57\u5217\u306E\u691C\u7D22\u3084\u64CD\u4F5C\u306B\u4F7F\u7528\
  \u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8907\
  \u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u306E\u51E6\u7406\u306B\u304A\u3044\u3066\u305D\
  \u306E\u52B9\u7387\u6027\u3068\u67D4\u8EDF\u6027\u304B\u3089\u3001PowerShell\u3067\
  \u306E\u30C7\u30FC\u30BF\u691C\u8A3C\u3001\u89E3\u6790\u3001\u5909\u63DB\u306A\u3069\
  \u306E\u30BF\u30B9\u30AF\u306Bregex\u3092\u6D3B\u7528\u3057\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？

正規表現（regex）は検索パターンを形成する文字の列であり、主に文字列の検索や操作に使用されます。プログラマーは、複雑なパターンの処理においてその効率性と柔軟性から、PowerShellでのデータ検証、解析、変換などのタスクにregexを活用します。

## 方法：

PowerShellでは、`-match`、`-replace`、`-split` オペレーターなどを使用して、正規表現を使ったアクションを実行できます。いくつかの例を探ってみましょう：

### 文字列がパターンに一致するかチェックするために `-match` を使用
このオペレーターはパターンが文字列内に見つかった場合は `$true` を、そうでない場合は `$false` を返します。

```powershell
"hello world" -match "\w+orld"
# 出力: True
```

### マッチした値の抽出
自動変数 `$matches` にアクセスすることで、マッチした値を抽出できます。

```powershell
if ("I have 100 apples" -match "\d+") {
    "Number found: " + $matches[0]
}
# 出力: Number found: 100
```

### 置換のために `-replace` を使用
`-replace` オペレーターは、指定した置換文字列でパターンの全出現箇所を置換します。

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# 出力: foo qux qux
```

### `-split` で文字列を分割
文字列を、regexパターンに基づいて文字列のサブ配列に分割します。

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# 出力: The quick brown fox jumps
```

### 高度なパターンマッチング
PowerShellは `[regex]` クラスを通じて、より複雑なregex操作をサポートしており、`Matches()`、`Replace()`、`Split()` などのメソッドにアクセスできます。

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# 出力: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# 出力: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# 出力: one two three four
```

これらの例は、データ操作とパターンマッチングにおいて、PowerShellでの正規表現の力と汎用性を示しています。regexを活用することで、プログラマーは効率的に複雑なテキスト処理を実行できます。
