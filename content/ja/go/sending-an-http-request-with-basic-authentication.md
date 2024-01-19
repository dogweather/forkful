---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---
title: Goを用いたHTTPリクエストの基本認証
---
## 何となぜ？
HTTPリクエストの基本認証は、ユーザ名とパスワードを用いて特定のリソースを保護する方法です。プログラマがこれを行う主な理由は、秘密情報を保護し、許可されたユーザだけがリソースにアクセスできるようにするためです。

## やり方
基本認証を使用してHTTPリクエストを送信する方法については、以下のGoコードスニペットを参照してください。

```Go
package main

import (
	"net/http"
	"fmt"
)

func main() {
	req, err := http.NewRequest("GET", "https://your-url.com", nil)
	if err != nil {
		log.Fatalln(err)
	}

	req.SetBasicAuth("username", "password")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatalln(err)
	}

	defer resp.Body.Close()

	fmt.Println("Response status:", resp.Status)
}
```
このコードを走らせると、以下のようにレスポンスステータスが表示されます。
```Go
Response status: 200 OK
```
## ディープダイブ
基本認証はHTTP/1.0から存在しており、ユーザ名とパスワードをBase64エンコード形式で送信します。しかしセキュリティ面から見ると、Base64エンコードは非常に弱く、容易にデコード可能なため、HTTPSによる暗号化を追加することが推奨されます。

また、OAuth2やJWTなどのより安全な認証方法が利用可能です。これらの認証方式は、大規模なWebアプリケーションやマイクロサービスのアーキテクチャに適しています。

```SetBasicAuth```はGoの```http```パッケージに含まれており、リクエストヘッダに認証情報を自動的に追加します。これは、手作業でヘッダを設定するよりも簡単な方法で、リクエストのボディに影響を与えません。

## 関連情報
以下のリンクは、HTTP基本認証とその代替についての追加情報を提供します。

- [Go公式ドキュメンテーション: httpパッケージ](https://golang.org/pkg/net/http/)
- [MDN Web Docs: HTTP認証](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [JWT認証についての詳細なガイド](https://jwt.io/introduction/)
- [OAuth2についての理解](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)