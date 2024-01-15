---
title:                "ベーシック認証を用いたhttpリクエストの送信"
html_title:           "C: ベーシック認証を用いたhttpリクエストの送信"
simple_title:         "ベーシック認証を用いたhttpリクエストの送信"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストに基本認証を使用して情報を送信することの利点を説明します。HTTPリクエストはサーバーに情報を取得するよう要求するものであり、基本認証によってセキュリティを強化することができます。 

## 方法

基本認証を使用してHTTPリクエストを送信する方法を示すため、以下のコード例をご覧ください。 

```C  
#include <stdio.h>  
#include <curl/curl.h>

int main(void) {  
    CURL *curl;  
    CURLcode res;  

    curl = curl_easy_init();
    if(curl) {
        // 送信する情報を設定
        char *user = "username";  
        char *password = "password";  

        // HTTPリクエストを送信するためのオプションを設定
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com"); // 送信先のURL
        curl_easy_setopt(curl, CURLOPT_USERNAME, user);  // ユーザー名を設定
        curl_easy_setopt(curl, CURLOPT_PASSWORD, password);  // パスワードを設定

        // リクエストを実行
        res = curl_easy_perform(curl);

        // リクエストが成功した場合、レスポンスを確認する
        if(res == CURLE_OK)
            printf("Request sent successfully!");
        else
            printf("Request failed.");

        // セッションを終了
        curl_easy_cleanup(curl);
  }
  return 0;
}
``` 
コードブロック内に、ユーザー名とパスワードを指定することで、基本認証を使用することができます。上記の例では、CURLライブラリを使用してリクエストを送信していますが、他のHTTPリクエストライブラリでも同様の方法で基本認証を実装することができます。 

## ディープダイブ

基本認証は、HTTPリクエストを送信する際に重要なセキュリティ機能となります。ユーザー名とパスワードを指定することで、第三者が情報を取得することを防ぎ、データのやりとりを安全に行うことができます。基本認証は、今でも多くのWebサービスで使用されており、セキュリティを確保するために必要不可欠な機能です。 

## 関連リンク

- [C言語でHTTPリクエストを送信する方法](https://www.geeksforgeeks.org/http-request-in-c-using-libcurl/)
- [libcurlの公式ドキュメント](https://curl.haxx.se/libcurl/)
- [C言語でのHTTP通信の実装方法](https://pipiscrew.com/2018/02/cpp-restapi-http-restfull-testlinks-hssf-httpstatussheetfile/)
- [基本認証の仕組みの詳細](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)