---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:01:54.448703-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
基本認証を使用したHTTPリクエストの送信とは、ユーザー名とパスワードを使ってリクエストを認証する手法です。Webサービスへの安全なアクセスを提供するためにプログラマーが使用します。

## How to:
```Go
package main

import (
    "encoding/base64"
    "fmt"
    "net/http"
)

func main() {
    client := &http.Client{}
    req, _ := http.NewRequest("GET", "http://your-api.com/data", nil)

    // ユーザー名とパスワードをセット
    username := "user"
    password := "pass"
    // Basic認証の文字列を生成
    auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // リクエストヘッダに追加
    req.Header.Add("Authorization", "Basic "+auth)

    // HTTPリクエストを送信
    resp, _ := client.Do(req)
    defer resp.Body.Close()
    fmt.Println("Status Code:", resp.StatusCode) // ステータスコードを出力
}
```
Sample Output:
```
Status Code: 200
```

## Deep Dive
HTTP Basic認証はRFC 7617で定義されており、もっともシンプルな認証手法の一つです。しかし、クレデンシャルがBase64でエンコードされただけであるため、HTTPSなどの暗号化通信と組み合わせなければ安全とは言えません。代わりにOAuthなどのよりセキュアな認証手法も広く使われています。実装の面では、`http.Request` オブジェクトを生成し、`Authorization` ヘッダに認証情報を加えることで基本認証が行われます。

## See Also
- Goの公式ドキュメント: [http package](https://pkg.go.dev/net/http)
- RFC 7617, The 'Basic' HTTP Authentication Scheme: [RFC 7617](https://tools.ietf.org/html/rfc7617)
- Goを使った安全なHTTP通信の詳細: [Go by Example: HTTP Clients](https://gobyexample.com/http-clients)
