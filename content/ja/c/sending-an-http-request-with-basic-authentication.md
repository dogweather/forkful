---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:00:49.695917-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (なぜとは何か？)
HTTP要求で基本認証を使用する理由とは、サーバーと安全に情報を交換する方法です。プログラマーはデータ保護のためにこれを行います。

## How to: (方法)
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        struct curl_slist *chunk = NULL;
        chunk = curl_slist_append(chunk, "Authorization: Basic <encoded-credentials>");

        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);
        CURLcode res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        
        curl_slist_free_all(chunk);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
出力:
```
{ "status": "success", "data": "..." }
```

## Deep Dive (深堀り)
HTTP基本認証では、ユーザー名とパスワードがBase64でエンコードされます。1990年代初頭のHTTP 0.9以来、認証には常に用いられています。他の認証方法にはOAuthなどがありますが、基本認証は簡単で互換性が高いと評価されている。C言語でHTTPリクエストを送るには、libcurlというライブラリが必要で、基本認証のセットアップも簡潔です。

## See Also (参照)
- cURL libcurl - Library for transferring data with URLs: https://curl.se/libcurl/
- Base64 Encoding - Base64 encoding schemes: https://www.base64encode.org/
- HTTP Authentication - Basics of HTTP authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
