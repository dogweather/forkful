---
title:                "C++: **Httpリクエストの送信"
simple_title:         "**Httpリクエストの送信"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストを送るのか

HTTPリクエストを送る主な理由は、ウェブサイトやアプリケーションとの通信を可能にすることです。このように、ユーザーからのデータ送信やサーバーからの情報取得など、インターネット上で情報をやり取りするために不可欠な機能です。

## 方法

まずは、基本的なHTTPリクエストの送り方を見ていきましょう。

```C++
#include <iostream>
#include <httplib.h>

int main()
{
  // HTTPクライアントの作成
  httplib::Client client("http://example.com");
  
  // GETリクエストの送信
  auto res = client.Get("/hello");
  
  // レスポンスの表示
  std::cout << res->body << std::endl;
  
  // POSTリクエストの送信
  res = client.Post("/submit", "name=John&age=25", "application/x-www-form-urlencoded");
  
  // レスポンスの表示
  std::cout << res->body << std::endl;
  
  return 0;
}

```

上記のコードでは、httplibというライブラリを使ってHTTPリクエストを送っています。まずは```httplib.h```をインクルードし、サーバーのURLを指定してクライアントを作成します。次に、GETやPOSTメソッドを使ってリクエストを送り、レスポンスの内容を取得して表示しています。また、POSTリクエストではデータの形式を明示的に指定しています。

## ディープダイブ

HTTPリクエストの詳細を見ていきましょう。

HTTPリクエストは、指定したURLに対して要求を送ることで起こります。この要求には、ヘッダーとボディという2つの部分があります。ヘッダーにはリクエストの詳細な情報が入り、ボディにはデータを含めることができます。そのため、GETやPOSTなどのHTTPメソッドだけでなく、ヘッダーの情報やボディの内容を適切に設定することが重要です。

また、HTTPリクエストにはステータスコードというものがあります。これはサーバー側から返される数字で、リクエストが成功したかどうかやエラーの原因を示すことができます。

さらに、HTTPSを使ってセキュアな通信を行うこともできます。その場合は、「https://」をURLの先頭につけることで、通常のHTTPと同じようにリクエストを送ることができます。

## 他の記事を見る

[httplibの公式ドキュメント](https://github.com/yhirose/cpp-httplib)

[C++でWebアプリケーションを作る方法](https://www.cyberciti.biz/faq/how-to-make-http-get-post-request-with-curl-use-tutorial/)

[HTTPリクエストについて知る](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)

[GETとPOSTの違いを理解する](https://www.w3schools.com/tags/ref_httpmethods.asp)

[HTTPSを使って安全な通信を行う](https://www.cloudflare.com/learning/ssl/why-use-https/)