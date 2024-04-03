---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:02.729794-07:00
description: "\u65B9\u6CD5: Go\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\
  \u4FE1\u3057\u3001\u5FDC\u7B54\u3092\u51E6\u7406\u3059\u308B\u306B\u306F\u3001`net/http`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\
  \u3001\u7C21\u5358\u306AGET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3057\
  \u3001\u5FDC\u7B54\u3092\u8AAD\u307F\u53D6\u308B\u624B\u9806\u3092\u793A\u3057\u305F\
  \u30B9\u30C6\u30C3\u30D7\u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u306E\u4F8B\u3067\u3059\
  \uFF1A."
lastmod: '2024-03-13T22:44:41.382269-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3057\u3001\
  \u5FDC\u7B54\u3092\u51E6\u7406\u3059\u308B\u306B\u306F\u3001`net/http`\u30D1\u30C3\
  \u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  \u7C21\u5358\u306AGET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3057\u3001\
  \u5FDC\u7B54\u3092\u8AAD\u307F\u53D6\u308B\u624B\u9806\u3092\u793A\u3057\u305F\u30B9\
  \u30C6\u30C3\u30D7\u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u306E\u4F8B\u3067\u3059\uFF1A\
  ."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
