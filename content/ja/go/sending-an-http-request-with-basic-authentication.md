---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "Go: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何を & どうして？
HTTPリクエストを基本認証で送信するとは何かを説明する2〜3文と、プログラマーがそのことをする理由について説明します。

基本認証を使用してHTTPリクエストを送信するとは、サーバーにアクセスする際にユーザー名とパスワードを使用して認証することを意味します。プログラマーは、ポストやデータベースの内容を保護するために基本認証を使用することがあります。

## 方法：
```Go
package main

import (
  "fmt"
  "net/http"
  "log"
)

func main() {
  req, err := http.NewRequest("GET", "https://example.com/api", nil)
  if err != nil {
    log.Fatalln(err)
  }
  req.SetBasicAuth("username", "password")
  resp, err := http.DefaultClient.Do(req)
  if err != nil {
    log.Fatalln(err)
  }
  defer resp.Body.Close()
  fmt.Println(resp.Status)
}
```

上記のコードは、基本認証を使用してサーバーにGETリクエストを送信する方法を示しています。まず、```http.NewRequest```関数を使用してリクエストオブジェクトを作成し、```SetBasicAuth```関数を使用して認証情報を設定します。次に、```http.DefaultClient.Do```関数を使用してリクエストを送信し、レスポンスを受け取ります。最後に、デフォルトのHTTPクライアントを閉じます。

```Go
200 OK
```

上記のようなレスポンスが返されれば、基本認証を使用してHTTPリクエストを正常に送信できたことを意味します。

## 深堀り：
基本認証は、1990年代初頭に開発されました。当時は、安全でないHTTPプロトコルが主流であり、機密データを保護するために必要な手段でした。しかし、現在ではセキュリティの観点からあまり推奨されておらず、代わりにトークン認証やOAuthといったより安全な認証方法が使用されることが多くなってきています。

基本認証以外の認証方法を使用する場合は、HTTPリクエストヘッダーに認証情報を含める必要があります。そのため、基本認証はシンプルで直感的な方法として依然として人気があります。

基本認証は、HTTPリクエストのカスタマイズやトークンの生成といったさまざまな機能を備えた多数のライブラリでサポートされています。しかし、基本的な認証プロトコルであるため、安全性が強化されることはありません。そのため、セキュリティが重要なアプリケーションでは、より強固な認証方法を使用することが推奨されます。

## 関連リンク：
- [Go標準パッケージドキュメント-```net/http```](https://golang.org/pkg/net/http/)
- [基本認証とは？](https://www.nttpc.co.jp/technology/solution/column/vol5/)
- [トークン認証とは？](https://auth0.com/learn/token-authentication/)