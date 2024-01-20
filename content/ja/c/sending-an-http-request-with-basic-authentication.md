---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なにそれ？なんで？

HTTPリクエストに基本認証を引き合いに出すとは、サーバーに対してユーザー名とパスワードの入力を求めるものです。プログラマーはこれを使用して、サイトにアクセスする前にユーザーの認証を行います。

## 使い方：

以下にC言語を用いてHTTPリクエストに基本認証を付与するコード例を示します。
```C
#include <curl/curl.h>

int main()
{
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // HTTPリクエスト先を指定
        curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/data");

        // ユーザー名とパスワード（形式は「ユーザー名:パスワード」）
        curl_easy_setopt(curl, CURLOPT_USERPWD, "user:password");

        // HTTPリクエストを実行
        res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

このコードを実行すると、以下のような出力結果を得ることが可能です。

```
HTTP/1.1 200 OK
Date: Mon, 01 Jan 2020 00:00:00 GMT
```

## より深く

基本認証はHTTP/1.0の時代から存在し、ユーザー名とパスワードのバイト列をBase64でエンコードして送信します。ただし、未暗号化の通信で使われると、パケットキャプチャー等の手段で容易に解読されてしまいます。よってHTTPS通信で利用します。

基本認証の代替としては、digest認証、OAuth、JWTなどがあります。実装の詳細については、libcurlライブラリのドキュメントを参照してください。

## 参考記事：

- [libcurlのドキュメント](https://curl.haxx.se/libcurl/)
- [HTTP認証について](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)