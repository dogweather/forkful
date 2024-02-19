---
aliases:
- /ja/c/sending-an-http-request/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:40.613024-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u307E\u305F\u306F\u63D0\u51FA\
  \u3059\u308B\u305F\u3081\u306B\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u306B\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u4F5C\u6210\u3057\u3066\u9001\u4FE1\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001C\u8A00\u8A9E\u3092\u4F7F\u7528\u3057\u3066\u30A6\u30A7\u30D6API\u3068\
  \u5BFE\u8A71\u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\
  \u30A6\u30F3\u30ED\u30FC\u30C9\u3057\u305F\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u304B\u3089\u76F4\u63A5\u4ED6\u306E\u30CD\u30C3\u30C8\u30EF\u30FC\
  \u30AF\u30B5\u30FC\u30D3\u30B9\u3068\u901A\u4FE1\u3057\u305F\u308A\u3057\u307E\u3059\
  \u3002"
lastmod: 2024-02-18 23:08:55.346339
model: gpt-4-0125-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u307E\u305F\u306F\u63D0\u51FA\u3059\
  \u308B\u305F\u3081\u306B\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u306B\u30EA\u30AF\
  \u30A8\u30B9\u30C8\u3092\u4F5C\u6210\u3057\u3066\u9001\u4FE1\u3059\u308B\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001C\u8A00\u8A9E\u3092\u4F7F\u7528\u3057\u3066\u30A6\u30A7\u30D6API\u3068\u5BFE\
  \u8A71\u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\
  \u30F3\u30ED\u30FC\u30C9\u3057\u305F\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u304B\u3089\u76F4\u63A5\u4ED6\u306E\u30CD\u30C3\u30C8\u30EF\u30FC\u30AF\
  \u30B5\u30FC\u30D3\u30B9\u3068\u901A\u4FE1\u3057\u305F\u308A\u3057\u307E\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを送信することは、データを取得または提出するためにウェブサーバーにリクエストを作成して送信することを意味します。プログラマーは、C言語を使用してウェブAPIと対話したり、ウェブページをダウンロードしたり、アプリケーションから直接他のネットワークサービスと通信したりします。

## 方法:

C言語でHTTPリクエストを送信するには、C言語にはウェブプロトコルのための組み込みサポートがないため、libcurlのようなライブラリに頼ることが一般的です。ここにlibcurlを使用してGETリクエストを行う簡単な例を示します:

まず、システムにlibcurlがインストールされていることを確認してください。次に、必要なヘッダーを含め、ソースファイルでlibcurlライブラリにリンクしてください：

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init(); // libcurlハンドルを初期化
    if(curl) {
        // libcurlハンドルを受け取るURLを設定
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // データを取得するためのコールバックを定義
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); 
        
        // リクエストを実行し、resが戻り値を受け取る
        res = curl_easy_perform(curl);
        // エラーをチェック
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // 常にクリーンアップする
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

これを`gcc -o http_request http_request.c -lcurl`のようにコンパイルし、実行すれば、"http://example.com"への簡単なGETリクエストを実行できます。

### サンプル出力

この例ではサーバーのレスポンスを処理しないため、実行してもエラーメッセージ以外に目に見える出力は生成されません。受信したデータを処理するためのコールバック関数を統合することが、意味のある対話に不可欠です。

## 深堀り

CプログラムからHTTPリクエストを送信する概念は、Cの強力なネットワーキング機能と、C自体が組み込みの高水準インターネットプロトコルサポートを持たない低水準言語であるため、外部ライブラリと組み合わせて成り立っています。歴史的に、プログラマーは専用ライブラリの登場前に、ウェブサーバーと対話するためにCで手動でソケットプログラミングを使用しており、これは複雑で面倒なプロセスでした。

LibcurlはCの上に構築され、ソケットプログラミングとHTTPプロトコルの詳細を抽象化し、プロセスを合理化します。HTTP/HTTPSに限らず、FTP、SMTPなど多くのプロトコルをサポートし、Cでのネットワーキングプログラミングに対して多用途なツールとなっています。

CでのHTTPリクエスト用のlibcurlの使用は実用的ですが、現代のプログラミングはPython（requestsライブラリ）やJavaScript（Fetch API）のような組み込みサポートを備えた言語に傾倒しがちです。これらの代替手段は、直接のソケット操作と細かく調整されたライブラリ使用によるCでの細かい制御とパフォーマンスの最適化を犠牲にして、より簡単で読みやすい構文を提供します。

クリティカルなパフォーマンスアプリケーションや直接的なシステムレベルの対話が必要な場合、libcurlがウェブ通信の複雑さを平滑化することで、Cは依然として実行可能なオプションです。しかし、ほとんどの高水準のウェブ対話においては、より専門的なウェブプログラミング言語を探索する方が効率的かもしれません。
