---
title:                "「HTTPリクエストを送信する」"
html_title:           "C: 「HTTPリクエストを送信する」"
simple_title:         "「HTTPリクエストを送信する」"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストを送信するのか

HTTPリクエストを送信する理由はいくつかありますが、最も一般的なのはWebページやアプリケーションのデータを取得するためです。サーバーにリクエストを送信することで、必要な情報を受け取ることができます。

## 使い方

まず、C言語でのHTTPリクエストの送信方法を説明します。以下のように、必要なヘッダーをインクルードし、リクエストを送信するためのソケットを作成します。

```C
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>

int main(){
    int clientSocket = socket(AF_INET, SOCK_STREAM, 0);
}
```

次に、リクエストを送信するためのデータを作成します。これにはHTTPメソッドやリクエストの宛先、ヘッダーなどが含まれます。以下のように、文字列として作成し、send関数を使ってソケットにデータを送信します。

```C
char request[1024] = "GET /index.html HTTP/1.1\r\nHost: www.example.com\r\n\r\n";
send(clientSocket, request, strlen(request), 0);
```

最後に、サーバーからのレスポンスを受け取り、表示します。以下のように、recv関数を使ってソケットからデータを受信し、printf関数で表示します。

```C
char response[1024];
recv(clientSocket, response, 1024, 0);
printf("%s", response);
```

上記のコードでは、"www.example.com"のindex.htmlページをリクエストし、サーバーからのレスポンスを表示することができます。

## 詳細を深堀りする

実際のHTTPリクエストでは、ヘッダーにさまざまな情報を含めることができます。また、HTTPSを使用する場合はさらにセキュリティー上の処理が必要になります。さらに詳しい情報を知りたい場合は、C言語でのHTTPリクエストの実装について調べてみてください。

## 関連情報

- [C言語入門](https://www.tohoho-web.com/ex/html/c.html)
- [HTTP リクエストとレスポンス](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [C言語でのソケット通信の基本](https://sis.sraoss.co.jp/tips/network_basic/conn_basic_c/basic_c01.html)