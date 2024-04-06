---
date: 2024-01-20 18:00:24.427296-07:00
description: "How to: (\u65B9\u6CD5) PowerShell\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u308B\u306B\u306F\u3001`Invoke-WebRequest` \u3084 `Invoke-RestMethod`\
  \ \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u3044\u307E\u3059\u3002\
  \u7C21\u5358\u306A\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.255744-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PowerShell\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\
  \u308B\u306B\u306F\u3001`Invoke-WebRequest` \u3084 `Invoke-RestMethod` \u30B3\u30DE\
  \u30F3\u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u3044\u307E\u3059\u3002\u7C21\u5358\u306A\
  \u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
