---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:32.407025-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/downloading-a-web-page.md"
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
