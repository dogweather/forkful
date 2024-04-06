---
date: 2024-01-20 18:01:21.049212-07:00
description: "How to: \u6DF1\u3044\u30C0\u30A4\u30D6 \u57FA\u672C\u8A8D\u8A3C\u306F\
  \u3001RFC 7617\u3067\u5B9A\u7FA9\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u30E6\
  \u30FC\u30B6\u30FC\u30CD\u30FC\u30E0\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30B3\
  \u30ED\u30F3\u3067\u9023\u7D50\u3057\u3001Base64\u3067\u30A8\u30F3\u30B3\u30FC\u30C9\
  \u3057\u307E\u3059\u3002\u6BD4\u8F03\u7684\u5358\u7D14\u3060\u304C\u3001HTTPS\u3092\
  \u4F7F\u308F\u306A\u3044\u3068\u60C5\u5831\u6F0F\u6D29\u306E\u30EA\u30B9\u30AF\u304C\
  \u3042\u308A\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.363095-06:00'
model: gpt-4-1106-preview
summary: "\u6DF1\u3044\u30C0\u30A4\u30D6 \u57FA\u672C\u8A8D\u8A3C\u306F\u3001RFC 7617\u3067\
  \u5B9A\u7FA9\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u30E6\u30FC\u30B6\u30FC\u30CD\
  \u30FC\u30E0\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30B3\u30ED\u30F3\u3067\u9023\
  \u7D50\u3057\u3001Base64\u3067\u30A8\u30F3\u30B3\u30FC\u30C9\u3057\u307E\u3059\u3002\
  \u6BD4\u8F03\u7684\u5358\u7D14\u3060\u304C\u3001HTTPS\u3092\u4F7F\u308F\u306A\u3044\
  \u3068\u60C5\u5831\u6F0F\u6D29\u306E\u30EA\u30B9\u30AF\u304C\u3042\u308A\u307E\u3059\
  ."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
