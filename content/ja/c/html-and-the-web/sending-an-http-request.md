---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:40.613024-07:00
description: "\u65B9\u6CD5: C\u8A00\u8A9E\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\
  \u9001\u4FE1\u3059\u308B\u306B\u306F\u3001C\u8A00\u8A9E\u306B\u306F\u30A6\u30A7\u30D6\
  \u30D7\u30ED\u30C8\u30B3\u30EB\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30B5\
  \u30DD\u30FC\u30C8\u304C\u306A\u3044\u305F\u3081\u3001libcurl\u306E\u3088\u3046\u306A\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u983C\u308B\u3053\u3068\u304C\u4E00\u822C\u7684\
  \u3067\u3059\u3002\u3053\u3053\u306Blibcurl\u3092\u4F7F\u7528\u3057\u3066GET\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u884C\u3046\u7C21\u5358\u306A\u4F8B\u3092\u793A\u3057\
  \u307E\u3059:\u2026"
lastmod: '2024-03-13T22:44:42.792232-06:00'
model: gpt-4-0125-preview
summary: "C\u8A00\u8A9E\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\
  \u308B\u306B\u306F\u3001C\u8A00\u8A9E\u306B\u306F\u30A6\u30A7\u30D6\u30D7\u30ED\u30C8\
  \u30B3\u30EB\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\
  \u304C\u306A\u3044\u305F\u3081\u3001libcurl\u306E\u3088\u3046\u306A\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u306B\u983C\u308B\u3053\u3068\u304C\u4E00\u822C\u7684\u3067\u3059\u3002\
  \u3053\u3053\u306Blibcurl\u3092\u4F7F\u7528\u3057\u3066GET\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u884C\u3046\u7C21\u5358\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
