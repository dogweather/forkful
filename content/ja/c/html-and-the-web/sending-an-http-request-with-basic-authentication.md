---
title:                "基本認証を使用してHTTPリクエストを送信する"
aliases:
- /ja/c/sending-an-http-request-with-basic-authentication/
date:                  2024-02-03T18:09:38.287168-07:00
model:                 gpt-4-0125-preview
simple_title:         "基本認証を使用してHTTPリクエストを送信する"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何を、なぜ？
Cで基本認証を用いてHTTPリクエストを送信することは、ユーザーの資格情報をBase64でエンコードして含めたAuthorizationヘッダーを持つHTTPリクエストを作成することを含みます。これは、プログラムによって制限されたリソースにアクセスできるようにするための、シンプルな認証レイヤーをHTTPリクエストに追加する一般的な方法です。

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
