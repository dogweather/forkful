---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:06.033726-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.794552-06:00'
model: gpt-4-0125-preview
summary: "C\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\
  \u30FC\u30C9\u3059\u308B\u306B\u306F\u3001libcurl\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u4F7F\u3046\u65B9\u6CD5\u304C\u4E00\u822C\u7684\u3067\u3059\u3002\u3053\u308C\u306F\
  \u3001\u52B9\u7387\u7684\u3067\u30DD\u30FC\u30BF\u30D6\u30EB\u306A\u30AF\u30E9\u30A4\
  \u30A2\u30F3\u30C8\u5074URL\u8EE2\u9001\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\u3059\
  \u3002\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306Blibcurl\u304C\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB\u3055\u308C\u3066\u30EA\u30F3\u30AF\u3055\u308C\u3066\u3044\u308B\u3053\
  \u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002\u4EE5\u4E0B\u306F\
  \u3001libcurl\u3092\u4F7F\u7528\u3057\u3066Web\u30DA\u30FC\u30B8\u306E\u30B3\u30F3\
  \u30C6\u30F3\u30C4\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u65B9\u6CD5\
  \u3092\u793A\u3059\u4F8B\u3067\u3059\uFF1A."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## どのようにして：
Cでウェブページをダウンロードするには、libcurlライブラリを使う方法が一般的です。これは、効率的でポータブルなクライアント側URL転送ライブラリです。プロジェクトにlibcurlがインストールされてリンクされていることを確認してください。以下は、libcurlを使用してWebページのコンテンツをダウンロードする方法を示す例です：

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // libcurlイージーセッションを初期化
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // 受信データを書き込むためのコールバック
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // データを書き込むファイルポインタを設定

        res = curl_easy_perform(curl); // ファイルダウンロードを実行
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        }

        /* 常にクリーンアップ */
        curl_easy_cleanup(curl); // イージーセッションをクリーンアップ
        fclose(fp); // ファイルストリームを閉じる
    }
    return 0;
}
```
サンプル出力（コンソールでの表示出力なし）：このコードは、指定されたURLのコンテンツをダウンロードして、`downloaded_page.html`という名前のファイルに保存します。ダウンロードしたコンテンツを見るために、プログラムのディレクトリを確認してください。

## より深く：
歴史的に、Cでウェブコンテンツをダウンロードすることは、手動でのソケットプログラミングとHTTPプロトコルの処理を必要とし、より手間がかかりました。Libcurlはこれらの複雑さを抽象化し、Web上でのデータ転送のための堅牢で高レベルのAPIを提供します。

LibcurlはCでのHTTPリクエストを簡素化しますが、`requests`ライブラリを持つPythonや、様々なHTTPクライアントライブラリを持つJavaScript（Node.js）のような現代のプログラミング言語は、Web通信で一般的に使用されるJSONや他のデータ形式のためのより直感的な構文と組み込みサポートを提供するかもしれません。しかし、Cとlibcurlは、効率性、細かい制御、または既存のCコードベースへの統合が重要なシステムにおいて、高性能かつ安定した解決策を提供します。Cとlibcurlが単にウェブページをダウンロードするだけでなく、FTP、SMTPなど、遥かに多くのことに使用できるため、プログラマーのツールキットで多用途なツールとなっていることも注目に値します。
