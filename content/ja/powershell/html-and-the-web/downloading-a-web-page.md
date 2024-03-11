---
date: 2024-01-20 17:44:32.407025-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3063\u3066\u306E\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\
  \u306E\u60C5\u5831\u3092\u624B\u306B\u5165\u308C\u308B\u30D7\u30ED\u30BB\u30B9\u3060\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u5206\u6790\u3001\
  \u76E3\u8996\u3001\u30C6\u30B9\u30C8\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u3084\
  \u308B\u3093\u3060\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.987200-06:00'
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3063\u3066\u306E\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\
  \u60C5\u5831\u3092\u624B\u306B\u5165\u308C\u308B\u30D7\u30ED\u30BB\u30B9\u3060\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u5206\u6790\u3001\u76E3\
  \u8996\u3001\u30C6\u30B9\u30C8\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u3084\u308B\
  \u3093\u3060\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (何して、なんで？)
Webページをダウンロードするってのは、インターネット上の情報を手に入れるプロセスだ。プログラマーはデータ分析、監視、テストのためにこれをやるんだ。

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
