---
title:                "HTTPリクエストの送信"
html_title:           "Go: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？
HTTPリクエストを送るとは何かと、プログラマーがそれを行う理由を説明する二、三の文。

HTTPリクエストを送るとは、インターネット上のサーバーに対しデータを要求することです。プログラマーはこの方法を使ってデータを取得したり、サーバーにコマンドを送ったりすることができます。

## 方法：
下記のようなコードブロック内のコーディング例とサンプル出力。

```Go
// インポートは必須
import (
    "fmt"
    "net/http"
)

// リクエストを作成する
req, err := http.NewRequest("GET", "https://example.com", nil)

// リクエストを送信し、レスポンスを取得する
resp, err := http.DefaultClient.Do(req)

// エラー処理
if err != nil {
    fmt.Println(err)
}

// レスポンスのステータスコードを表示
fmt.Println(resp.Status)
```

### 出力：
```200 OK```

## 詳しく見る：
(1)歴史的文脈、(2)代替手段、そして(3)HTTPリクエストの実装についての詳しい情報。

### 歴史的文脈：
HTTPリクエストは1991年にティム・バーナーズ＝リーによって開発されました。当初はテキストベースのプロトコルでしたが、現在では画像やビデオなどのメディアでも使用されています。HTTP/2やHTTPSのような新しいバージョンやセキュリティプロトコルも登場しました。

### 代替手段：
HTTPリクエストの代替手段としては、WebSocketやgRPCなどのプロトコルが存在します。それぞれ異なる用途や利点があり、開発者は必要に応じて適切なプロトコルを選択することができます。

### HTTPリクエストの実装：
Go言語では、```net/http```パッケージを使用してHTTPリクエストを実装することができます。このパッケージには様々なメソッドや関数が用意されており、簡単にリクエストを作成、送信、取得することができます。

## 関連情報：
関連する情報源へのリンク。

- [Go言語公式ドキュメント](https://golang.org/pkg/net/http/)
- [HTTPリクエストの仕組みを理解する](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [WebSocketとは？](https://developer.mozilla.org/ja/docs/Web/API/WebSocket/Introduction)