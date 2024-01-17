---
title:                "HTTPリクエストの送信"
html_title:           "PowerShell: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何をしているか？ 何故するのか？
HTTPリクエストを送信することは、ウェブページやウェブサービスにアクセスする方法です。プログラマーは、データのやりとりや情報の取得を目的として、HTTPリクエストを送信します。

## 方法：
以下のコードは、PowerShellを使用して、任意のウェブサイトからHTTPリクエストを送信し、レスポンスを取得する方法の例です。
```
# ウェブサイトのURLを指定
$url = "https://www.example.com"
# HTTPリクエストを送信
$request = Invoke-WebRequest -Uri $url
# レスポンスからHTMLソースコードを取得
$request.Content
```
このコードを実行すると、指定したURLから取得したHTMLソースコードが表示されます。

## 詳細：
- HTTPリクエストは、1990年代にWebサイトを構築するための標準的なプロトコルとして開発されました。
- PowerShell以外にも、cURLやPythonのRequestsライブラリなど、他のエンドポイントからHTTPリクエストを送信するためのツールもあります。
- Invoke-WebRequestコマンドレットは、PowerShell 3.0以降でサポートされています。また、PowerShell 6.0以降では、Invoke-WebMethodというコマンドレットが使用可能です。

## 関連情報：
- [PowerShell公式ドキュメント](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-6)
- [HTTPリクエストの仕組みを理解する](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [cURL公式サイト](https://curl.haxx.se/)