---
title:                "Python: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストを送信する必要があるのか

HTTPリクエストを送信することで、インターネット上のさまざまな情報やAPIにアクセスすることができます。これにより、ウェブサイトやアプリケーションの機能を拡張することができるだけでなく、データの取得や更新など、さまざまなタスクを実行することができます。

## 方法

Pythonを使用して、HTTPリクエストを送信する方法をご紹介します。まず、requestsライブラリをインポートします。

```Python
import requests
```

次に、リクエストのURLと認証情報を設定します。基本認証を行う場合は、usernameとpasswordを設定する必要があります。

```Python
url = "https://example.com/api"
username = "user123"
password = "password123"
```

そして、requestsライブラリのgetメソッドを使用して、リクエストを送信します。

```Python
response = requests.get(url, auth=(username, password))
```

最後に、响应を確認し、必要な処理を行います。

```Python
print(response.text)
```

上記の例では、APIから返されたデータをコンソールに表示していますが、実際にはそれに応じた処理を行うことができます。

## ディープダイブ

基本認証を使用したHTTPリクエストを送信する場合、サーバーとのセッションを確立する際に、ユーザーが指定したユーザー名とパスワードが必要になります。この時、ユーザー名とパスワードはBase64エンコードされた形式でリクエストヘッダーに含まれるため、セキュリティ上の理由からHTTPSを使用することが推奨されています。

また、基本認証以外にも、トークンを使用した認証やOAuth認証など、さまざまな認証方法がありますので、必要に応じて適切な方法を選択する必要があります。

## さらに読む

- [Requestsライブラリ公式ドキュメント](https://requests.readthedocs.io/en/latest/)
- [Base64エンコードについて](https://ja.wikipedia.org/wiki/Base64)
- [HTTP Basic認証について](https://tools.ietf.org/html/rfc7617)