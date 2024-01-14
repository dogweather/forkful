---
title:                "Go: 基本認証を使用してhttpリクエストを送信する方法"
simple_title:         "基本認証を使用してhttpリクエストを送信する方法"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信する理由を説明します。基本認証とは、ユーザー名とパスワードを使用してウェブサーバーにアクセスするための方法です。

## 方法

まずは、`net/http`パッケージをインポートしましょう。

```Go
import (
	"net/http"
)
```

次に、`http.NewRequest()`関数を使用して基本認証を含むリクエストを作成します。以下のコードでは、URLと認証用のユーザー名とパスワードが設定されています。

```Go
// URL
url := "https://example.com/api/users"
// 認証用のユーザー名とパスワード
username := "user"
password := "password"

// 基本認証を含むリクエストを作成
req, err := http.NewRequest("GET", url, nil)
req.SetBasicAuth(username, password)
```

そして、作成したリクエストを`http.Client`を使用して送信します。

```Go
// http.Clientを作成
client := &http.Client{}
// リクエストを送信
resp, err := client.Do(req)
```

コードの出力結果は以下のようになります。

```
Status: 200 OK
Body: {"message": "Hello, user!"}
```

## 詳細を調べる

基本認証には、ユーザー名とパスワードを平文で送信するため、セキュリティリスクがあります。そのため、HTTPSを使用して暗号化した接続を行うことが推奨されます。また、ユーザー名とパスワードの代わりにトークンを使用する方法もあります。

基本認証は、特に外部のAPIにアクセスする場合によく使用されます。しかし、セキュリティ上のリスクを考慮し、SSL接続を使用することを強くお勧めします。

## はじめにもどる

基本認証を使用してHTTPリクエストを送信する方法は、Go言語を使用する上で重要なことです。今回紹介した方法を覚えておくと、外部のAPIにアクセスする際に役立つことでしょう。

## 他にも参考になるリンク

- [net/httpパッケージドキュメント（英語）](https://golang.org/pkg/net/http/)
- [OWASP：基本認証（英語）](https://owasp.org/www-community/attacks/Basic_Authentication)
- [Go製の基本認証ライブラリ：basicauth（英語）](https://github.com/goji/httpauth/tree/master/basicauth)