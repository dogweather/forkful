---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:38.287168-07:00
description: "\u65B9\u6CD5\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.795386-06:00'
model: gpt-4-0125-preview
summary: "C\u3067\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u3066HTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u306B\u306F\u3001libcurl\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\
  \u3002\u3053\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001\u4EBA\u6C17\u304C\u3042\
  \u308A\u3001\u591A\u7528\u9014\u3067\u4F7F\u3044\u3084\u3059\u3044\u30AF\u30E9\u30A4\
  \u30A2\u30F3\u30C8\u30B5\u30A4\u30C9URL\u8EE2\u9001\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3067\u3059\u3002HTTP\u304A\u3088\u3073HTTPS\u3092\u542B\u3080\u3055\u307E\u3056\
  \u307E\u306A\u30D7\u30ED\u30C8\u30B3\u30EB\u3092\u6271\u3046\u305F\u3081\u3001\u79C1\
  \u305F\u3061\u306E\u4ED5\u4E8B\u3092\u7C21\u5358\u306B\u3057\u307E\u3059\u3002\u9032\
  \u3080\u524D\u306B\u3001\u30B7\u30B9\u30C6\u30E0\u306Blibcurl\u304C\u30A4\u30F3\u30B9\
  \u30C8\u30FC\u30EB\u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\
  \u3066\u304F\u3060\u3055\u3044\u3002\u4EE5\u4E0B\u306F\u3001\u57FA\u672C\u8A8D\u8A3C\
  \u3092\u7528\u3044\u3066GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\
  \u308B\u65B9\u6CD5\u3092\u793A\u3059\u57FA\u672C\u4F8B\u3067\u3059\uFF1A."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u3066HTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B"
weight: 45
---

## 方法：
Cで基本認証を用いてHTTPリクエストを送信するには、libcurlライブラリを使用する必要があります。このライブラリは、人気があり、多用途で使いやすいクライアントサイドURL転送ライブラリです。HTTPおよびHTTPSを含むさまざまなプロトコルを扱うため、私たちの仕事を簡単にします。進む前に、システムにlibcurlがインストールされていることを確認してください。以下は、基本認証を用いてGETリクエストを送信する方法を示す基本例です：

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // リクエストが送信されるURL
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // 基本認証の使用を可能にする
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // 基本認証のためのユーザー名とパスワードを提供する
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // GETリクエストを実行する
        res = curl_easy_perform(curl);

        // エラーの確認
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // 常にクリーンアップする
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
上記の例では、`"http://example.com/resource"`、`"username"`、および`"password"`を実際のURL、ユーザー名、パスワードに置き換えてください。

このコードは、`CURL`オブジェクトを初期化し、URLを設定し、HTTP Basic認証を有効にし、資格情報を指定します。それからリクエストを送信し、自身をクリーンアップします。成功した場合、要求されたリソースが取得されます。エラーがある場合は、stderrにプリントされます。

サンプル出力（成功した認証とリソースアクセスを前提としているが）は、この例がリクエストの送信を主に示しているため、プログラムによって直接表示されない可能性があります。レスポンスをプリントするには、プログラムをHTTPレスポンスデータを処理するように拡張する必要があります。

## 深堀り：
Cで基本認証を用いたHTTPリクエストの送信は、示されたとおり、その堅牢さとシンプルさのためにlibcurlライブラリを活用します。歴史的に、そのようなライブラリなしにCだけでHTTPリクエストを作ることは、低レベルのソケットプログラミングやHTTPヘッダーの手動構築を含む、面倒でエラーが起きやすい作業でした。

基本認証自体は、ウェブの初期の日からの方法です。それは、容易にデコード可能な形式（Base64）で資格情報を送信するもので、プレーンテキストチャネル上では本質的に安全ではありません。現代のアプリケーションでは、特に機密データに対しては、OAuth 2.0やJWT（JSON Web Tokens）のような、より安全な認証方法を好むことが多いです。

しかし、内部の、それほど重要でないシステムや、便利さが安全上の懸念を上回るクイックアンドダーティなスクリプトにおいては、基本認証が使用され続けています。さらに、暗号化された接続（HTTPS）と組み合わされた場合、そのシンプルさは、高度なセキュリティメカニズムがそれほど必要でない迅速な開発、テスト、または自動化作業において、利点となります。

最先端のセキュリティが譲れない文脈においては、トークンベースの認証のような代替手段を優先すべきです。それにもかかわらず、libcurlを通じてCで基本認証を実装する方法を理解することは、さまざまな認証方法とプロトコルに適応できる基礎的なスキルを提供し、ウェブ開発におけるセキュリティ、便利さ、およびアプリケーション要件間の微妙なトレードオフを反映しています。
