---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ?
HTTPリクエストを送信するとは、サーバーに情報を要求または送信するためのメッセージです。これにより、プログラマーはウェブサーバーと双方向でデータ交換ができます。

## どうやって:
C++を使用してHTTPリクエストを送信する基本的なコードを次に示します。 まず、必要なライブラリを含めることから始めます。

```C++
#include <cpprest/http_client.h>
#include <cpprest/filestream.h>
```

次に、HTTPリクエストを送信するシンプルな関数を作成します。

```C++
pplx::task<void> HTTPPostAsync()
{
    web::http::client::http_client client(U("http://example.com"));

    // Post 방식으로 전송할 request 메시지를 만듭니다.
    web::http::http_request request(web::http::methods::POST);
    request.set_body(U("Sample Data"));

    return client.request(request)
        .then([](web::http::http_response response)
    {
        // Print the status code.
        std::cout << response.status_code() << std::endl;
    });
}

int main()
{
    HTTPPostAsync().wait();
    return 0;
}
```
サンプルの出力で成功メッセージを確認することができます。

```
200
```

## ディープダイブ
HTTPリクエストの送信は、早くも1991年からWebが生まれたときからありました。C++でのHTTPリクエストの送信には他にもオプションがあります。libcurl、QtのQNetworkAccessManagerなどがここで言及できます。

この記事では、Microsoftのcpprestsdkを示しましたが、これは非同期プログラミングパターンに基づいています。これは効率的なリソース利用を提供する一方で、クラシックな同期I/Oよりも少し学ぶのが難しくなる可能性があります。

## 参照リンク
1. Microsoft cpprestsdkのドキュメンテーション: [ここ](https://github.com/microsoft/cpprestsdk)を参照
2. libcurlについての詳細: [ここ](https://curl.haxx.se/libcurl/c/)から確認できます
3. QtのQNetworkAccessManagerの使い方: [ここ](https://doc.qt.io/qt-5/qnetworkaccessmanager.html)を参照してください