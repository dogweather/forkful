---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
初めに、HTTPリクエストを送信することの利点は何でしょうか？HTTPリクエストを送信することによって、インターネット上の他のサイトやサービスとのデータのやりとりが可能になります。これは、ウェブサイトから情報を取得するだけでなく、APIを使用してリソースを作成したり更新したりするのにも役立ちます。

## How To
まず、BashでHTTPリクエストを送信する方法を見てみましょう。下記の例を参考にしてください。

```Bash
#!/bin/bash

# curlコマンドを使用してGoogleのウェブページにHTTPリクエストを送信する
curl https://www.google.com

# レスポンスヘッダーを含め、詳細な情報を表示するには-vオプションを使用
curl -v https://www.google.com

# POSTリクエストを送信するためには、-Xオプションを使用してメソッドを指定し、-dオプションを使用してデータを指定する
curl -X POST -d "username=John&password=pass123" https://www.example.com/login
```

上記の例では、`curl`コマンドを使用してHTTPリクエストを送信しています。さまざまなオプションを使用して、リクエストの詳細をカスタマイズすることができます。

```Bash
# オプションの一覧を表示するには-hオプションを使用
curl -h

# リクエスト先のURLを指定するには-Oオプションを使用
curl -O https://www.example.com/file.zip

# 新しいファイル名を指定するには-oオプションを使用
curl -o new_filename.zip https://www.example.com/file.zip
```

## Deep Dive
上記の例では`curl`コマンドを使用してHTTPリクエストを送信しましたが、実際にはBashだけでリクエストを送信することも可能です。Bashで使用できる`curl`コマンドは、Libcurlと呼ばれるC言語で書かれたライブラリを使用しています。

Libcurlは、HTTPやFTPなどの様々なプロトコルに対応しており、BashだけでHTTPリクエストを送信することができます。しかし、直接Libcurlを使用するよりも、`curl`コマンドを使用する方が簡単です。

## See Also
今回紹介した`curl`コマンドやLibcurl以外にも、HTTPリクエストを送信するためのさまざまなツールがあります。以下のリンクから詳細を確認してみてください。

- [HTTPie](https://httpie.org/)
- [wget](https://www.gnu.org/software/wget/)
- [axel](http://axel.alioth.debian.org/)