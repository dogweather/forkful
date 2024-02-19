---
aliases:
- /ja/go/sending-an-http-request/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:02.729794-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1\u3068\u306F\u3001\
  Go\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304B\u3089\u30A6\u30A7\u30D6\
  \u30B5\u30FC\u30D0\u30FC\u3001API\u3001\u307E\u305F\u306F\u4ED6\u306EHTTP\u30D9\u30FC\
  \u30B9\u306E\u30B5\u30FC\u30D3\u30B9\u3078\u306E\u547C\u3073\u51FA\u3057\u3092\u958B\
  \u59CB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30EA\u30BD\u30FC\u30B9\u3068\
  \u5BFE\u8A71\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\
  \u308A\u3001\u30D5\u30A9\u30FC\u30E0\u3092\u9001\u4FE1\u3057\u305F\u308A\u3001\u30A4\
  \u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u3092\u4ECB\u3057\u3066\u4ED6\u306E\u30B5\u30FC\
  \u30D3\u30B9\u3068\u901A\u4FE1\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.476564
model: gpt-4-0125-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1\u3068\u306F\u3001Go\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304B\u3089\u30A6\u30A7\u30D6\u30B5\u30FC\
  \u30D0\u30FC\u3001API\u3001\u307E\u305F\u306F\u4ED6\u306EHTTP\u30D9\u30FC\u30B9\u306E\
  \u30B5\u30FC\u30D3\u30B9\u3078\u306E\u547C\u3073\u51FA\u3057\u3092\u958B\u59CB\u3059\
  \u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30EA\u30BD\u30FC\u30B9\u3068\u5BFE\u8A71\
  \u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\
  \u30D5\u30A9\u30FC\u30E0\u3092\u9001\u4FE1\u3057\u305F\u308A\u3001\u30A4\u30F3\u30BF\
  \u30FC\u30CD\u30C3\u30C8\u3092\u4ECB\u3057\u3066\u4ED6\u306E\u30B5\u30FC\u30D3\u30B9\
  \u3068\u901A\u4FE1\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信とは、Goアプリケーションからウェブサーバー、API、または他のHTTPベースのサービスへの呼び出しを開始することを意味します。プログラマーは、ウェブリソースと対話したり、データを取得したり、フォームを送信したり、インターネットを介して他のサービスと通信するためにこれを行います。

## 方法:

GoでHTTPリクエストを送信し、応答を処理するには、`net/http`パッケージを使用します。以下は、簡単なGETリクエストを送信し、応答を読み取る手順を示したステップバイステップの例です：

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // リソースのURLを定義
    url := "http://example.com"

    // http.Getを使用してGETリクエストを送信
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // 関数が終了するときにレスポンスボディを閉じる
    defer resp.Body.Close()

    // レスポンスボディを読み取る
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // レスポンスボディを文字列に変換して印刷する
    fmt.Println(string(body))
}
```

サンプル出力（簡潔さのために短縮）:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

フォームデータを持つPOSTリクエストを送信するには、`http.PostForm`を使用できます：

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // URLとフォームデータを定義
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // フォームデータを持つPOSTリクエストを送信
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // 応答を読み取って印刷
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## 詳細解説

Goの`net/http`パッケージは、HTTPサーバーと対話するための強力かつ柔軟な方法を提供します。その設計は、単純さ、効率性、堅牢性を重視するGoの特徴を反映しています。元々、JSONやXMLのペイロードを扱う機能は、リクエストボディを手作業で作成し、適切なヘッダーを設定する必要がありました。Goが進化するにつれて、これらのタスクをさらに簡素化する高レベルのパッケージがコミュニティによって開発されました。これには、ルーティング用の`gorilla/mux`やJSON操作の`gjson`などがあります。

GoのHTTPクライアントの注目すべき側面の一つは、`http.Client`や`http.Request`のようなインターフェースと構造体の使用であり、これにより広範なカスタマイズとテストが可能になっています。例えば、リクエストのタイムアウトやパフォーマンスのために接続を維持するために`http.Client`を変更することができます。

簡単なHTTPインタラクションのための代替手段として、"Resty"や"Gentleman"のようなサードパーティライブラリの使用が考えられます。これらのパッケージは、より高レベルの抽象化をHTTPリクエストに提供し、一般的なタスクをより簡潔にします。しかし、より複雑またはユニークなHTTPインタラクションシナリオを扱うためには、`net/http`パッケージを理解し、利用することが不可欠であり、Goの並行性機能や強力な標準ライブラリを完全に活用するための基盤を提供します。
