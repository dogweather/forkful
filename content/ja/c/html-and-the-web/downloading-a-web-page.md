---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:06.033726-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.794552-06:00'
model: gpt-4-0125-preview
summary: "C\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\
  \u30FC\u30C9\u3059\u308B\u3053\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\
  \u30C8\u7D4C\u7531\u3067\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30B3\u30F3\u30C6\
  \u30F3\u30C4\u306B\u30D7\u30ED\u30B0\u30E9\u30E0\u7684\u306B\u30A2\u30AF\u30BB\u30B9\
  \u3057\u3001\u51E6\u7406\u3084\u30AA\u30D5\u30E9\u30A4\u30F3\u4F7F\u7528\u306E\u305F\
  \u3081\u306B\u30ED\u30FC\u30AB\u30EB\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3057\
  \u3070\u3057\u3070\u3001Web\u30B5\u30FC\u30D3\u30B9\u3092\u6D88\u8CBB\u3057\u305F\
  \u308A\u3001Web\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\
  \u30F3\u30B0\u3057\u305F\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u304B\u3089\u76F4\u63A5\u30AA\u30F3\u30E9\u30A4\u30F3\u30EA\u30BD\u30FC\u30B9\u3068\
  \u3084\u308A\u53D6\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002."
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
