---
date: 2024-01-20 18:01:21.049212-07:00
description: "\u4F55\u3068\u306F\u4F55\u304B\uFF1F\u305D\u3057\u3066\u306A\u305C\uFF1F\
  \ HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u4ED8\u3051\
  \u3066\u9001\u308B\u3053\u3068\u3067\u3001\u30A2\u30AF\u30BB\u30B9\u3092\u8A31\u53EF\
  \u3055\u308C\u305F\u30E6\u30FC\u30B6\u30FC\u3060\u3051\u304C\u60C5\u5831\u3092\u53D6\
  \u5F97\u3067\u304D\u308B\u3088\u3046\u306B\u5236\u9650\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u3063\u3066\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u3092\u4FDD\u3061\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.510562-07:00'
model: gpt-4-1106-preview
summary: "\u4F55\u3068\u306F\u4F55\u304B\uFF1F\u305D\u3057\u3066\u306A\u305C\uFF1F\
  \ HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u4ED8\u3051\
  \u3066\u9001\u308B\u3053\u3068\u3067\u3001\u30A2\u30AF\u30BB\u30B9\u3092\u8A31\u53EF\
  \u3055\u308C\u305F\u30E6\u30FC\u30B6\u30FC\u3060\u3051\u304C\u60C5\u5831\u3092\u53D6\
  \u5F97\u3067\u304D\u308B\u3088\u3046\u306B\u5236\u9650\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u3063\u3066\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u3092\u4FDD\u3061\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why?
何とは何か？そしてなぜ？

HTTPリクエストに基本認証を付けて送ることで、アクセスを許可されたユーザーだけが情報を取得できるように制限します。プログラマーはこれを使ってセキュリティを保ちます。

## How to:
どうやって？

```C++
#include <iostream>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;
    
    curl = curl_easy_init();
    if(curl) {
        std::string userPwd = "username:password"; // ベーシック認証のユーザー名とパスワード

        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPwd.c_str());
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

        res = curl_easy_perform(curl);
        
        if (res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        } else {
            std::cout << readBuffer << std::endl; // HTTPレスポンスの内容を表示
        }
        
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
出力例:
```
<html>
<body>
<p>認証された内容がここに表示されます。</p>
</body>
</html>
```

## Deep Dive:
深いダイブ

基本認証は、RFC 7617で定義されています。ユーザーネームとパスワードをコロンで連結し、Base64でエンコードします。比較的単純だが、HTTPSを使わないと情報漏洩のリスクがあります。

代わりにOAuthなどのよりセキュアな手法が利用されることもあります。ただ、簡単な内部システムやテスト用のケースでは、基本認証が使われることも多いです。

CURLはC言語のライブラリですが、C++からも使えます。設定は`curl_easy_setopt`を通じて行い、`CURLOPT_USERPWD`でユーザーネームとパスワードを設定します。これにより、HTTPヘッダに認証情報が組み込まれます。

## See Also:
参照してください

- libcurl公式ドキュメント: https://curl.se/libcurl/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Base64エンコード: https://ja.wikipedia.org/wiki/Base64
- OAuth: https://oauth.net/
