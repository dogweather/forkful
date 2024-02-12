---
title:                "HTTPリクエストの送信"
aliases:
- /ja/go/sending-an-http-request/
date:                  2024-02-03T18:09:02.729794-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTTPリクエストの送信"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
