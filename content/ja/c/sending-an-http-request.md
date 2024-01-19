---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ ?
HTTPリクエストを送信するとは、サーバーに特定のデータを要求する方法です。これはよくウェブページのデータを取得したり、ウェブサービスと交信したりするために行います。

## 実際にどうやるの？
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                        curl_easy_strerror(res));
        }

        curl_easy_cleanup(curl);
    }
    return 0;
}
```
上記のコードはhttp://example.comにHTTPリクエストを送信し、応答を表示します。

## 詳説
HTTPリクエストの送信は、インターネットが生まれたときから行われている基本的な操作です。代替手段としては、HTTPS（セキュアなHTTP）、FTP（ファイル転送プロトコル）などがありますが、目的により適切な手段を選びます。

この実装では、Open Sourceのライブラリであるlibcurlを使用しています。libcurlは、HTTPをはじめとする様々なプロトコルの通信をシンプルに抽象化し、より高度な機能を提供します。

## 参考リンク
1. libcurlの公式ページ：[https://curl.haxx.se/libcurl/](https://curl.haxx.se/libcurl/)
2. HTTPの詳細な仕様（英語）: [https://tools.ietf.org/html/rfc2616](https://tools.ietf.org/html/rfc2616)