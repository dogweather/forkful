---
title:                "「HTTPリクエストの送信」"
html_title:           "Python: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何をし、なぜ？
HTTPリクエストを送ることは、わかりやすく言えばウェブサーバーに「何か」を言ってそのサーバーが「何か」を返してくれるプロセスです。プログラマーがHTTPリクエストを送る理由はさまざまですが、例えばウェブアプリケーションでデータを取得したり、外部のAPIと通信したりするために使います。

## 方法：
Pythonのリクエストライブラリを使ってHTTPリクエストを送る方法はとても簡単です。例えば、下記のコードを使うことでウェブサイトからHTMLコンテンツを取得することができます。


```Python
import requests

r = requests.get('https://example.com')
print(r.text) # ウェブサイトのコンテンツを表示
```

このように、```requests```ライブラリの```get```関数を使うことで、指定したURLからGETリクエストを送り、その結果を取得することができます。また、POSTやPUTなどの異なるタイプのリクエストも同じように送ることができます。

## 詳細を掘り下げる：
HTTPリクエストは、1990年代の初めにWorld Wide Webの成立に伴い開発されました。それ以前のプロトコルでは、情報の交換は「一方向」でしたが、HTTPは「双方向」の通信を可能にしました。代替手段としては、telnetやFTPなどがありますが、それらは一般的にはより複雑で、セキュリティの問題もあります。

HTTPリクエストの実装に関して、Pythonの```requests```ライブラリ以外にも様々な選択肢があります。例えば、```urllib```や```http.client```モジュールなどがありますが、それらはより低レベルのインターフェースを提供しています。

## 関連情報：
- [HTTPリクエストとは？初心者向けに分かりやすく解説](https://www.sejuku.net/blog/79896)
- [PythonリクエストライブラリでHTTPリクエストの送信方法を学ぶ](https://www.sejuku.net/blog/122053)