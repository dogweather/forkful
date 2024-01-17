---
title:                "基本認証でhttpリクエストを送信する"
html_title:           "Python: 基本認証でhttpリクエストを送信する"
simple_title:         "基本認証でhttpリクエストを送信する"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# こんにちは！ Python プログラマーさんたち！

この記事では、Python で HTTP リクエストをプログラムする際に、基本認証を使ってリクエストを送信する方法について説明します。基本認証は古くから使われてきた認証方式ですが、今でも多くのプログラマーが利用しています。プログラマーたちは、基本認証を使ってセキュアで使いやすいアプリケーションを作ることができるからです。

## 何をする？ なぜする？

基本認証は、ユーザー名とパスワードを使ってWebサイトにログインしたり、プライベートな情報にアクセスするための一般的な方法です。プログラマーたちは基本認証を使って、ユーザーがアプリケーションにアクセスする際に、安全でプライベートな情報を送ることができるようにしています。

## 使い方：

Python で HTTP リクエストを送信するには、 `requests` モジュールを使用します。基本認証を使用するには、`auth` パラメータを使ってユーザー名とパスワードを指定します。

```Python
import requests

URL = "https://example.com"
USERNAME = "user123"
PASSWORD = "password123"

response = requests.get(URL, auth=(USERNAME, PASSWORD))

print(response.status_code)  # 200: 成功
```

## 深く掘り下げる：

基本認証は、HTTP プロトコルの仕様の一部として定義されています。サーバー側は、ユーザーが提供したユーザー名とパスワードを検証し、認証に成功すればリソースにアクセスすることを許可します。しかし、基本認証はセキュアではないため、多くの現代的な認証方式が開発されました。

もし基本認証が使えない場合には、代わりに API キーを使用したり、OAuth 認証を使用することができます。しかし、基本認証は使いやすく汎用性が高いため、まだ多くのアプリケーションで使われています。

また、基本認証の詳細については、RFC7617ドキュメントを確認することができます。

## さらに読んでみる：

- [Requests: HTTP Library for Python](https://requests.readthedocs.io/en/master/)
- [HTTP Basic Authentication](https://www.geeksforgeeks.org/http-basic-authentication-python/) - Python で基本認証を実装する方法についてのチュートリアル。
- [Python: Making HTTP Requests](https://realpython.com/python-requests/) - Python で HTTP リクエストを作るためのチュートリアル。