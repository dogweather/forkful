---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T17:59:56.465443-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

category:             "Go"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るって？それは、ウェブサーバーにデータを要求または送信することです。なぜ使うか？データを受け取ったり、ウェブサービスと対話したりするために必要です。

## How to: (やり方)
以下のGoコードサンプルは、HTTP GETリクエストを送る方法を示しています。

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    response, err := http.Get("https://jsonplaceholder.typicode.com/todos/1")
    if err != nil {
        fmt.Printf("The HTTP request failed with error %s\n", err)
    } else {
        data, _ := ioutil.ReadAll(response.Body)
        fmt.Println(string(data))
    }
}
```

実行結果:

```
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive (深堀り)
HTTPリクエストの送信はHTTPの基本的な操作です。1991年に登場したHTTPは、ウェブの成長と共に進化してきました。GETとPOSTは最も一般的なメソッドです。Go言語の標準ライブラリ`net/http`は、この機能を簡単に実装するためのツールを提供しています。例えば、`http.Get`関数は内部でTCP接続を管理し、HTTPリクエストを行い、レスポンスを取得します。クライアントのカスタマイズ、タイムアウトの設定、ヘッダーの追加なども可能です。

他の言語やライブラリが提供するアプローチとしては、cURL、Postman、およびNode.jsの`axios`などがあります。また、HTTP/2のサポート、非同期リクエスト、ストリーム処理などの実装詳細は、Goのバージョンによって異なる場合があります。

## See Also (関連情報)
以下のリンクでは、HTTPリクエストについてのさらなる情報を提供しています：

- Goの公式ドキュメント: [http](https://pkg.go.dev/net/http)
- JSONPlaceholder (テスト用API): [JSONPlaceholder](https://jsonplaceholder.typicode.com/)
- HTTPに関するさらなる学習: [MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/HTTP)
