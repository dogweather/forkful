---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTP要求に基本認証を使用するとは、HTTP要求を送信する際にユーザ名とパスワードを含むことです。プログラマがこれを行う主な理由は、特定のウェブリソースへのアクセスを制御するためです。

## 使い方：

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://example.com', auth=HTTPBasicAuth('user', 'pass'))

print(response.status_code)
```
このコードは、指定したurl('https://example.com')に対してHTTP GETリクエストを送信します。'user'と'pass'はそれぞれユーザ名とパスワードです。応答ステータスコードが出力されます。

## ディープダイブ：

基本認証によるHTTPリクエストはウェブの早期から存在し、特にAPIの初期のバージョンでよく見られました。しかし、現在ではよりセキュアなOAuthなどの認証方式がより一般的になってきています。

基本認証の主な欠点は、パスワードが平文で送信されることで、単独で使うにはセキュリティが弱いとされています。そのため、通常はHTTPSと組み合わせて使われます。

基本認証に代わる方法としては、Digest認証やToken認証、OAuthがあります。これらは各々異なる形式でクライアントの認証情報をサーバに伝達します。

また、requestsライブラリは内部でurllib3を使用してHTTPリクエストを行います。それに対して認証情報はHTTPヘッダーに追加され、サーバに送信されます。

## 参考資料：

1. Python `requests`の公式ドキュメンテーション：https://requests.readthedocs.io/en/latest/
2. 基本認証についてのWikipediaの記事：https://ja.wikipedia.org/wiki/基本認証
3. 具体的な代替認証戦略：https://auth0.com/blog/what-are-the-most-common-types-of-http-authentication/