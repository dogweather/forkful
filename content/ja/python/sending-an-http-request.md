---
title:                "「httpリクエストの送信」"
html_title:           "Python: 「httpリクエストの送信」"
simple_title:         "「httpリクエストの送信」"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを送信することに関わる理由は、あなたのPythonプログラムがWebの情報を取得したい場合です。HTTPリクエストは、ウェブサイトからデータを取得するための基本的な方法です。

## 使い方
```python
import requests

# GETリクエストの例
response = requests.get('https://www.google.com')
print(response.text)
```
上記のコードは、Pythonのrequestsライブラリを使用してGoogleのウェブサイトからデータを取得する方法を示しています。`get()`メソッドは、引数としてリクエストを送信するURLを指定し、レスポンスを`response`変数に格納します。その後、`text`属性を使用してレスポンスのHTMLコンテンツを取得し、`print`文で出力します。

```python
import requests

# POSTリクエストの例
data = {'username': 'example', 'password': 'password'}
response = requests.post('https://example.com/login', data=data)
print(response.status_code)
```
上記のコードは、POSTリクエストを使用してログイン情報を送信する方法を示しています。`post()`メソッドは、引数としてリクエストを送信するURLとデータを指定し、レスポンスを`response`変数に格納します。`status_code`属性を使用して、リクエストのステータスコードを取得し、`print`文で出力します。

## 深堀り
HTTPリクエストには、様々なオプションやパラメータがあります。例えば、ヘッダーを指定してリクエストをカスタマイズすることもできます。また、リクエストのレスポンスが成功したかどうかや、何を返しているかを確認することも重要です。Pythonのrequestsライブラリには、さまざまなメソッドや属性があり、これらを正しく理解することで、より高度なHTTPリクエストの制御が可能になります。

## 参考リンク
- [Python公式ドキュメント: requestsライブラリ](https://docs.python.org/3/library/requests.html)
- [Requests: HTTP for Humans](https://requests.readthedocs.io/en/master/)
- [curlとの違いを解説！使えるrequestsライブラリ【入門】](https://qiita.com/toyotarou2/items/f59f2da1b3fba69a09fd)
- [Python初心者でもわかる！Requestsモジュールでapiを叩いてみる](https://qiita.com/nancoc/items/67a372ef77063feb15c2)

## さらに読み込み
さらにHTTPリクエストについて知りたい方は、上記のリンクを参考にしてみてください。また、APIの使用やWebスクレイピングなど、Pythonを使用したさまざまなWeb関連のプログラミングにも挑戦してみてください。