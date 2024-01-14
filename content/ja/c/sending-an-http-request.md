---
title:                "C: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストを送信するのか

HTTPリクエストを送信することは、プログラマーにとって非常に重要なスキルです。これは、ウェブサイトから必要な情報やデータを取得するために使用されるからです。例えば、オンラインショッピングサイトでは、HTTPリクエストが使用されていることで、商品の在庫や価格を確認することができます。

## 方法

それでは、C言語を使用してHTTPリクエストをどのように送信するのか、見ていきましょう。下記のコードブロックに、HTTPリクエストを送信するための基本的な例を示します。

```C
#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <netinet/in.h>

int main(){
    // ホスト名やポート番号を指定
    char *hostname = "www.example.com";
    int portno = 80;

    // ソケットを作成
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if(sockfd < 0){
        // ソケット作成エラー
        perror("ソケット作成エラー");
        exit(1);
    }

    // ホスト名をIPアドレスに変換
    struct hostent *server;
    server = gethostbyname(hostname);
    if(server == NULL){
        // ホスト名変換エラー
        fprintf(stderr, "ホスト名変換エラー\n");
        exit(1);
    }

    // サーバーとの接続
    struct sockaddr_in serv_addr;
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((char *)server->h_addr_list[0], (char *)&serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(portno);

    if(connect(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0){
        // 接続エラー
        perror("接続エラー");
        exit(1);
    }

    // HTTPリクエストの送信
    char *request = "GET / HTTP/1.1\r\n\r\n";
    if(send(sockfd, request, strlen(request), 0) < 0){
        // 送信エラー
        perror("送信エラー");
        exit(1);
    }

    // サーバーからのレスポンスの受信
    char buffer[1024];
    bzero(buffer, 1024);
    if(recv(sockfd, buffer, 1024, 0) < 0){
        // 受信エラー
        perror("受信エラー");
        exit(1);
    }

    // レスポンスの表示
    printf("%s\n", buffer);

    // ソケットを閉じる
    close(sockfd);

    return 0;
}
```

上記のコードでは、www.example.comに対してGETメソッドのHTTPリクエストを送信し、レスポンスを受信しています。このように、プログラム内でHTTPリクエストを送信することで、サーバーからのデータを取得することができます。

## ディープダイブ

HTTPリクエストを送信する際には、さまざまな種類のリクエストがあり、さらにさまざまなオプションもあります。例えば、GETメソッド以外にもPOSTやPUTなどのメソッドが存在します。また、リクエストヘッダーやボディーを設定することで、さらに詳細なリクエストを送信することもできます。

さらに重要な点として、セキュリティがあります。HTTPリクエストは平文で送信されるため、機密性の高