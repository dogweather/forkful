---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

「ウェブページのダウンロード」とは、ウェブページのコンテンツを自分のコンピュータ上に保存することを指します。これは、プログラマがウェブコンテンツを解析したり、後で参照するために実行します。

## 手順：

以下に示すのは、C言語を使ってウェブページをダウンロードするサンプルコードとその出力です:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void){
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

このコードを実行すると、`http://example.com`の内容が出力されます。

## ディープダイブ：

### 歴史の視点：
C言語でウェブページをダウンロードする方法は、電子メール、FTP、TELNETなどをサポートする総合的なライブラリ「libcurl」の登場により現実のものとなりました。

### 代替方法：
libcurl以外に、`wget`, `WinINet`, `libwww`などのライブラリやコマンドラインツールも利用できます。

### 実装詳細：
curlは内部でソケットを作成し、HTTPリクエストを送信してレスポンスを受け取ることでウェブページをダウンロードします。この処理は、サーバから送信されたデータをデコードし、必要な情報を取り出すことが可能です。

## 参考リンク：

1. [libcurl 公式ドキュメンテーション](https://curl.se/libcurl/c/)
2. [C言語とネットワーク](https://www.geekhideout.com/urlcode.shtml)
3. [HTTPについて理解する](https://developer.mozilla.org/ja/docs/Web/HTTP)