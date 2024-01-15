---
title:                "基本認証を使ってhttpリクエストを送信する"
html_title:           "Go: 基本認証を使ってhttpリクエストを送信する"
simple_title:         "基本認証を使ってhttpリクエストを送信する"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

HTTPリクエストにBasic認証を使って送信する理由を最大2文で説明する。 

誰か別のウェブサイトやアプリケーションにアクセスする際、そのウェブサイトやアプリケーションのサーバーが正しい認証情報を持ったユーザーであることを確認する必要があるためです。

## How To

```Go
func makeRequest() {
    // HTTPリクエストを設定
    req, err := http.NewRequest("GET", "https://example.com", nil)

    // 認証情報をヘッダーに追加
    req.SetBasicAuth("username", "password")

    // リクエストを送信
    client := http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }

    // リクエストの結果を表示
    fmt.Println(resp)
}
```

上記のコード例では、Go言語でHTTPリクエストを作成し、Basic認証を使って認証情報をヘッダーに追加しています。また、リクエストを送信し、レスポンスを表示しています。

## Deep Dive

Basic認証は、クライアントがユーザー名とパスワードをBase64エンコードしてリクエストヘッダーのAuthorizationフィールドに含めることで認証を行います。サーバーはこのヘッダーを受け取り、デコードして認証情報を検証します。この認証方法はセキュリティ面で不十分だと考えられ、今ではより強力な認証方法が推奨されています。

## See Also

- [Go言語でHTTPリクエストを送る方法](https://www.yoheim.net/blog.php?q=20160702)
- [Go言語でBasic認証を使ったHTTPリクエストをする方法](https://qiita.com/miyohide/items/d96aa3abf369a2ca9875)