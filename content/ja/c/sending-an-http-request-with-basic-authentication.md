---
title:                "C: 基本認証付きのhttpリクエストの送信"
simple_title:         "基本認証付きのhttpリクエストの送信"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信する理由は、セキュリティ上の理由です。基本認証を使用することで、リクエストを送信する際に必要なパスワードを暗号化することができます。

## 方法

基本認証を使用してHTTPリクエストを送信するには、まずHTTPヘッダーに認証情報を追加する必要があります。以下のコード例では、ユーザー名とパスワードを変数に格納し、`curl`コマンドを使用してリクエストを送信しています。

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  char *username = "user";
  char *password = "pass";

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api");
    curl_easy_setopt(curl, CURLOPT_USERNAME, username);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, password);
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

上記のコードでは、`curl_easy_setopt`関数を使用して`CURLOPT_USERNAME`と`CURLOPT_PASSWORD`を設定し、`curl_easy_perform`関数を使用してリクエストを実行しています。実行すると、サーバーからのレスポンスが得られます。

## ディープダイブ

基本認証を使用してHTTPリクエストを送信する場合、以下のようなことに注意する必要があります。

- パスワードは平文で送信されるため、セキュリティ上のリスクがあります。HTTPSを使用することで、通信を暗号化することができます。
- 送信先のサーバーが認証情報を保存している場合、リクエストごとに認証情報を再度入力する必要があります。
- 他の認証方法と比べると、セキュリティレベルが低いため、より強力な認証方法を検討することも重要です。

## 関連リンク

- [curlで基本認証付きのHTTPリクエストを送信する方法](https://curl.haxx.se/libcurl/c/smtp-tls.html)
- [HTTP認証の種類とその仕組み](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [セキュリティについて学べるオンラインコース](https://www.coursera.org/courses?query=security%20online%20course)