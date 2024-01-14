---
title:                "C++: Basic認証を使用して httpリクエストを送信する"
simple_title:         "Basic認証を使用して httpリクエストを送信する"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信する理由は、サーバーから保護されたリソースにアクセスする必要があるためです。基本認証は、ユーザーがユーザー名とパスワードを使用してサーバーにログインするための一般的な方法です。

## 方法

基本認証を使用してHTTPリクエストを送信するための基本的なC++コードの例を示します。

```C++
#include <iostream>
#include <curl/curl.h> // curlライブラリのインクルード

using namespace std;

int main() {
    CURL *curl;
    CURLcode res;

    // curlオプションを設定
    curl = curl_easy_init(); // curl初期化
    if (curl) {
       curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/protected_resource"); // リクエスト先のURLを指定
       curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC); // basic認証を指定
       curl_easy_setopt(curl, CURLOPT_USERNAME, "username"); // ユーザー名を指定
       curl_easy_setopt(curl, CURLOPT_PASSWORD, "password"); // パスワードを指定

       // リクエストの実行
       res = curl_easy_perform(curl);

       // エラー処理
       if (res != CURLE_OK)
           cout << "Error: " << curl_easy_strerror(res) << endl;

       // curl終了処理
       curl_easy_cleanup(curl);
    }

    return 0;
}
```

上記のコードを実行すると、指定したURLのリソースに対して基本認証が行われ、その後リクエストが実行されます。もし認証が成功すると、サーバーからリソースのデータが返されます。

## ディープダイブ

基本認証は、HTTPリクエストを送信する方法の一つですが、セキュリティ上の問題があります。パスワードは平文で送信されるため、認証情報が傍受されると危険です。そのため、よりセキュアな認証方式を使用することをお勧めします。また、基本認証を使用する場合は、HTTPSプロトコルを使用することで情報を暗号化することができます。

## 関連リンク

- [C++でHTTPリクエストを送信する方法](https://docs.microsoft.com/ja-jp/cpp/libraries/curl?view=msvc-160)
- [Basic認証についての詳細](https://ja.wikipedia.org/wiki/Basic%E8%AA%8D%E8%A8%BC)
- [HTTPSプロトコルについての詳細](https://en.wikipedia.org/wiki/HTTPS)