---
title:                "ベーシック認証を使用してhttpリクエストを送信する"
html_title:           "C++: ベーシック認証を使用してhttpリクエストを送信する"
simple_title:         "ベーシック認証を使用してhttpリクエストを送信する"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
なぜ？
HTTPリクエストで基本認証を行う必要があるのでしょうか？
基本認証は、Webサーバーとのやり取りにおいて、セキュリティを確保するための重要な手段の一つです。特定のWebリソースにアクセスするためには、認証情報を提供する必要があります。そのため、基本認証を用いてHTTPリクエストを送信することによって、安全にリソースにアクセスすることができます。

## How To
どのようにすればいいの？
まず、基本認証を行うためには、コンピューター上で実行されるプログラムを用いて、HTTPリクエストを送信する必要があります。以下のようなC++のコードを使って、基本認証を行ったHTTPリクエストを送信することができます。

```
#include <iostream>
#include <curl/curl.h>

using namespace std;

int main()
{
  // curlセッションの初期化
  CURL *curl;
  curl = curl_easy_init();
  
  if(curl)
  {
    // URLの設定
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    
    // 認証情報の設定
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
    
    // HTTPリクエストの実行
    CURLcode res = curl_easy_perform(curl);
    
    // エラーハンドリング
    if(res != CURLE_OK)
    {
      cerr << "An error has occurred: " << curl_easy_strerror(res) << endl;
      curl_easy_cleanup(curl);
      return 1;
    }
    
    // curlのクリーンアップ
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

上記のコードでは、curlライブラリを使用して、基本認証のためのユーザー名とパスワードを設定し、HTTPリクエストを送信しています。成功した場合は、WebサイトのHTMLページが取得され、それがcoutに出力されます。

## Deep Dive
詳細な情報
基本認証には、ユーザー名とパスワードを平文で送信するというセキュリティ上の問題があります。そのため、HTTPSを使うことで暗号化することが推奨されています。また、基本認証を行う際には、認証情報を保持するためのセッションを確立する必要があります。これにより、同じ認証情報を繰り返し送信することなく、複数のリクエストを行うことができます。

## See Also
詳細な情報が必要な場合は、以下のリンクを参考にしてください。

- [HTTP Basic認証の仕組みと実装方法](https://qiita.com/RyosukeTai/items/641d0675e2b8ab0db1aa)
- [HTTP Authentication](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [curl ライブラリのドキュメント](http://curl.se/libcurl/c/)