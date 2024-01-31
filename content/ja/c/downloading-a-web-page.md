---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:22.949050-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

Downloading a web page is about fetching the content of a page from the internet. Programmers do this to process or analyze data, automate tasks, or just to save for offline use.

ウェブページをダウンロードするとは、インターネットからページの内容を取得することです。プログラマーはこれをデータ処理や分析、タスクの自動化、あるいはオフライン使用のために行います。

## How to:
## 方法：

To download a web page in C, you'll typically use a library called cURL. This example shows you how:

C言語でウェブページをダウンロードするためには、通常cURLというライブラリが使われます。以下の例を見てみましょう：

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
    char outfilename[FILENAME_MAX] = "downloaded_page.html";
    
    curl = curl_easy_init();
    if(curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

Running this code will create a file named `downloaded_page.html` containing the HTML from "http://example.com".

このコードを実行すると、"http://example.com"からHTMLが含まれた`downloaded_page.html`というファイルが作成されます。

## Deep Dive
## 詳細な情報：

Before cURL, downloading web content was more manual and complex, often requiring socket programming. cURL simplifies the process with a library that handles many of the underlying HTTP protocol details.

cURLの前では、ウェブコンテンツのダウンロードはより手作業の多い複雑なもので、しばしばソケットプログラミングが必要でした。cURLは多くの根底にあるHTTPプロトコルの詳細を処理するライブラリを使いプロセスを単純化します。

There are alternatives to cURL, such as libcurl and WinINet on Windows, but cURL is widely supported across various platforms. When using cURL, consider the security implications and ensure to use HTTPS to protect sensitive data.

cURLの代わりにはlibcurlやWindowsのWinINet等がありますが、cURLは様々なプラットフォームで広くサポートされています。cURLを利用する際にはセキュリティの影響を考慮し、機密データを守るためHTTPSの使用を確実にしてください。

## See Also
## 関連リンク：

- cURL programming basics: https://curl.haxx.se/libcurl/c/libcurl-tutorial.html
- cURL official website: https://curl.haxx.se/
- HTTP protocol overview: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview

cURLプログラミングの基礎: https://curl.haxx.se/libcurl/c/libcurl-tutorial.html
cURL公式ウェブサイト: https://curl.haxx.se/
HTTPプロトコル概要: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
