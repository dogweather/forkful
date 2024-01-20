---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを送信するとは、あなたのアプリケーションがウェブサーバーと通信できるように指示を送ることを意味します。これはウェブサイトの情報を取得したり、APIを使用してデータを取得したり更新したりする際にプログラマが実行します。

## どうやって：

以下に、GoでHTTPリクエストを送信するための簡単な方法を示します。次のコードスニペットをご覧ください：

```Go
package main

import (
  "io/ioutil"
  "log"
  "net/http"
)

func main() {
  res, err := http.Get("http://example.com")
  if err != nil {
    log.Fatal(err)
  }
  defer res.Body.Close()

  body, err := ioutil.ReadAll(res.Body)
  if err != nil {
    log.Fatal(err)
  }
  log.Println(string(body))
}
```

このコードはhttp.Get関数を使ってhttp://example.comからHTTPリクエストを発行します。もし何か問題があれば、エラーがログに報告されます。

## ディープダイブ：

HTTPリクエストの送信はWeb開発の基礎であり、Go言語がこのタスクを容易に行えるように構築されています。Goの `net/http` パッケージは、HTTPクライアントとサーバーの機能を提供しています。

他の手法としては、`http.NewRequest` 関数を使ってリクエストを手動で作成し、`http.Client.Do` メソッドで送信することも可能です。

```Go
req, err := http.NewRequest("GET", "http://example.com", nil)
client := &http.Client{}
res, err := client.Do(req)
```

## 関連項目：

- [Goの公式ドキュメンテーションのnet/httpパッケージ](https://golang.org/pkg/net/http/)
- [Go by Example: HTTPクライアント](https://gobyexample.com/http-clients)