---
date: 2024-01-20 17:44:32.407025-07:00
description: "How to: (\u3084\u308A\u65B9) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.257900-06:00'
model: gpt-4-1106-preview
summary: ''
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
