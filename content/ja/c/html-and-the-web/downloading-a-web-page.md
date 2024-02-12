---
title:                "ウェブページのダウンロード"
aliases: - /ja/c/downloading-a-web-page.md
date:                  2024-02-03T17:56:06.033726-07:00
model:                 gpt-4-0125-preview
simple_title:         "ウェブページのダウンロード"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Cでウェブページをダウンロードすることは、インターネット経由でウェブページのコンテンツにプログラム的にアクセスし、処理やオフライン使用のためにローカルに保存することを意味します。プログラマーはしばしば、Webサービスを消費したり、Webコンテンツをスクレイピングしたり、アプリケーションから直接オンラインリソースとやり取りするためにこれを行います。

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
