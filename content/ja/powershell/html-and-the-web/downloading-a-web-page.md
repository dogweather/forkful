---
date: 2024-01-20 17:44:32.407025-07:00
description: "How to: (\u3084\u308A\u65B9) Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\
  \u30F3\u30ED\u30FC\u30C9\u3059\u308B\u6A5F\u80FD\u306F\u3001PowerShell\u304C\u767B\
  \u5834\u3057\u305F2006\u5E74\u304B\u3089\u5229\u7528\u53EF\u80FD\u306B\u306A\u3063\
  \u305F\u3002`Invoke-WebRequest`\u306FPowerShell\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.326057-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\
  \u30FC\u30C9\u3059\u308B\u6A5F\u80FD\u306F\u3001PowerShell\u304C\u767B\u5834\u3057\
  \u305F2006\u5E74\u304B\u3089\u5229\u7528\u53EF\u80FD\u306B\u306A\u3063\u305F\u3002\
  `Invoke-WebRequest`\u306FPowerShell v3.0\u3067\u767B\u5834\u3057\u3001HTML\u3084\
  XML\u306E\u5185\u5BB9\u3092\u30D1\u30FC\u30B9\u3057\u3066\u4F7F\u3044\u3084\u3059\
  \u304F\u3057\u305F\u3002\u4EE3\u66FF\u624B\u6BB5\u3068\u3057\u3066\u306F\u3001`System.Net.WebClient`\
  \ \u30AF\u30E9\u30B9\u3084`curl`\u30B3\u30DE\u30F3\u30C9\u304C\u3042\u308B\u3002\
  \u305F\u3060\u3001`Invoke-WebRequest`\u306F\u3088\u308APowerShell\u3089\u3057\u3044\
  \u4F7F\u3044\u52DD\u624B\u304C\u7279\u5FB4\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (やり方)
```PowerShell
# Invoke-WebRequest を使ってWebページの内容を取得する
$response = Invoke-WebRequest -Uri "http://example.com"

# コンテンツを表示する
$response.Content
```

サンプル出力:
```
<!DOCTYPE html>
<html>
<body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents.</p>
</body>
</html>
```

## Deep Dive (掘り下げ)
Webページをダウンロードする機能は、PowerShellが登場した2006年から利用可能になった。`Invoke-WebRequest`はPowerShell v3.0で登場し、HTMLやXMLの内容をパースして使いやすくした。代替手段としては、`System.Net.WebClient` クラスや`curl`コマンドがある。ただ、`Invoke-WebRequest`はよりPowerShellらしい使い勝手が特徴。

PowerShellでは、`Invoke-WebRequest`がWebページの内容を取得して、そのデータを扱いやすい形にしてくれる。スクリプトはページ内の特定のデータを抜き出したり、継続的なプロセスの一環でデータを収集したりするのに使える。

## See Also (参照)
- [Invoke-WebRequest documentation](http://go.microsoft.com/fwlink/?LinkID=217035)
- [Microsoft PowerShell Git repository for the latest updates](https://github.com/PowerShell/PowerShell)
