---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:24.427296-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストはWebサーバーに情報を求める方法です。データを取得したり、ウェブサービスと交信するためにプログラマーはこれを使います。

## How to: (方法)
PowerShellでHTTPリクエストを送るには、`Invoke-WebRequest` や `Invoke-RestMethod` コマンドレットを使います。簡単な例を見てみましょう。

```PowerShell
# シンプルなGETリクエスト
$response = Invoke-WebRequest -Uri "http://example.com"
$response.StatusCode
$response.Content

# JSON形式でPOSTリクエストを送信
$body = @{
    name = 'Taro'
    email = 'taro@example.com'
} | ConvertTo-Json
$response = Invoke-RestMethod -Method Post -Uri "http://example.com/api/users" -ContentType "application/json" -Body $body
$response
```

これらのコードはウェブサーバーからのレスポンスステータスコードや内容を表示します。

## Deep Dive (深掘り)
HTTPリクエストの送信は、ウェブの初期からの基本的な操作です。`Invoke-WebRequest` と `Invoke-RestMethod` は PowerShell 3.0 から導入されました。これらは、`curl` や `wget` のような伝統的なUNIXコマンドラインツールと似た動作をしますが、PowerShell 環境に統合されており、オブジェクトとして結果を操作できます。

代替として、.NET の `HttpClient` や `WebRequest` クラスを利用することも可能ですが、コードが増える傾向にあります。`Invoke-RestMethod` は主にREST APIとの交信に役立ち、JSON処理が自動化されている点が特徴です。

## See Also (関連項目)
- [Invoke-WebRequest公式ドキュメント](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Invoke-RestMethod公式ドキュメント](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)