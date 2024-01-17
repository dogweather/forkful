---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何もの & なぜ?
HTTPリクエストに基本認証を付加することとは何か、そしてなぜプログラマーがそれを行うのかを説明します。

HTTPリクエストに基本認証を付加することは、クライアントがサーバーに対して認証を行い安全な通信を確保するための手段です。プログラマーはこれを利用して、様々なAPIやWebサービスへのアクセスを制限したり、セキュリティを強化したりすることができます。

## 方法:
以下に、C言語を使ってHTTPリクエストに基本認証を付加する方法を示します。コードブロック内のサンプルコードと出力結果を確認してください。

```C
// 必要なヘッダーファイルをインクルードする
#include <stdio.h>
#include <curl/curl.h>

int main(void){
    
    // cURLの初期化
    CURL *curl;
    CURLcode res;
    
    // URLを指定する
    curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");
    
    // ユーザー名とパスワードを指定する
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
    
    // オプションを設定
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    
    // HTTPリクエストを送信して結果を出力する
    res = curl_easy_perform(curl);
    printf("%d\n", res);
    
    // cURLのクリーンアップ
    curl_easy_cleanup(curl);
    
    return 0;
}
```
```
0 // 成功した場合は0が出力されます
```

## 詳細を知る:
### 歴史的背景:
基本認証は、最初のHTTPの認証方式として1990年代に開発されました。その後、よりセキュアな認証方式であるダイジェスト認可やSSLが登場し、基本認証はより簡単な認証方式として利用されるようになりました。

### 代替手段:
基本認証以外にも、OAuthやOpenIDなどの認証方式があります。これらはよりセキュアで柔軟性がありますが、実装が複雑であり、基本認証に比べると時間やリソースがかかります。

### 実装の詳細:
基本認証は、HTTPリクエストのヘッダーにAuthorizationフィールドを追加することで行われます。このフィールドには、Base64エンコードされたユーザー名とパスワードの組み合わせが含まれます。サーバーはこの情報を解析し、ユーザーの認証を行います。

## 関連情報を見る:
- [cURL](https://curl.se/)
- [HTTP Basic Authentication](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [An Introduction to Basic HTTP Authentication](https://www.digitalocean.com/community/tutorials/understanding-basic-http-authentication)