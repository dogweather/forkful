---
title:                "HTTPリクエストを送信する"
html_title:           "Fish Shell: HTTPリクエストを送信する"
simple_title:         "HTTPリクエストを送信する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何か & 何故？

HTTPリクエストを送信するとは何か？それは、プログラマーがウェブサイトやウェブアプリケーションを操作したり、データを取得したりするために使う方法です。HTTPリクエストを送信することで、インターネット上のさまざまな情報にアクセスすることができます。

## やり方：

```
Fish Shellを使用してHTTPリクエストを送信する方法を見てみましょう。

最初に、以下のコマンドを入力します：

```
curl -i https://www.example.com/
```

これにより、指定したURLにGETリクエストが送信されます。そして、サーバーからのレスポンスを受け取り、その情報をターミナルに表示します。

```
HTTP/1.1 200 OK
Date: Sun, 01 Mar 2020 12:00:00 GMT
Server: Apache
Content-Length: 120
Content-Type: text/html; charset=UTF-8

<!DOCTYPE html>
<html>
<head>
<title>Welcome to Example.com</title>
</head>
<body>
<h1>Hello World!</h1>
</body>
</html>
```

このように、リクエストを送信することで、ウェブサイトのタイトルやHTMLコードなどの情報を取得することができます。

## 詳しく見る：

### 歴史的背景：

HTTPリクエストの送信方法には、さまざまな歴史的背景があります。最初のウェブサーバーは、HTTPリクエストを受け取って、ファイルを返すように設計されていました。しかし、今ではHTTPリクエストを使用して、データを取得するだけでなく、送信したり、削除したりすることもできるようになりました。

### 代替案：

HTTPリクエストを送信する方法としては、Fish Shell以外にもさまざまな方法があります。例えば、PythonのrequestsモジュールやNode.jsのAxiosモジュールなどがあります。ただし、これらのモジュールを使用するには、それぞれの言語を学習する必要があります。

### 実装の詳細：

Fish Shellでは、curlコマンドを使用することで、簡単にHTTPリクエストを送信することができます。具体的には、URLを指定し、任意のメソッド（GET、POST、PUT、DELETEなど）を使用することができます。

## 関連情報：

- [HTTPリクエストとは？](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [curlコマンドの使い方](https://curl.haxx.se/docs/manpage.html)
- [リクエストを送信する他の方法](https://github.com/ggreer/the_silver_searcher/wiki/Fish-Shell-パイプラインでリクエストを送信する方法)