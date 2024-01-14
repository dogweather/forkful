---
title:                "C: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

Webページをダウンロードするのに、なぜ私たちはプログラムを書くのでしょうか？先ず、ダウンロードするコンテンツを保存したいとか、自分のアプリケーションで使用したいかもしれません。また、インターネット接続が切れた時などに、ローカルにコンテンツを保存しておくためにも使用されることがあります。

## ダウンロードの方法

Webページをダウンロードするには、多くの方法がありますが、ここではC言語のプログラム例を紹介します。まずは、必要なヘッダーファイルをインクルードしましょう。

```
#include <stdio.h>
#include <curl/curl.h>
```

次に、ダウンロードしたいURLを指定します。ここでは、Googleのホームページを例に取ります。

```
char *url = "https://www.google.com";
```

以下のように、curlを使用してWebページをダウンロードできます。```file```にはダウンロードした内容が格納されます。

```
CURL *curl;
FILE *file;
CURLcode response;

curl = curl_easy_init();

if(curl) {
    file = fopen("google.html", "wb");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);
    response = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    fclose(file);
}
```

ダウンロードが完了したら、```curl_easy_cleanup()```でリソースを解放しましょう。

## 深堀り

プログラム例では、curlを使用してWebページをダウンロードしましたが、実際にはHTTPプロトコルでリクエストを送り、レスポンスを受け取っています。curlは非常に強力なライブラリであり、さまざまなカスタマイズが可能です。また、コード中の```CURLOPT_URL```や```CURLcode```などの定数についても、より詳しく調べることができます。

## 参考

* [curlコマンドの使い方](https://www.atmarkit.co.jp/ait/subtop/features/diycurl.html)
* [C言語でcurlを使ってhttpアクセスを行う](https://qiita.com/YoshikiIto/items/28dc7808bf3eae04681e)
* [curl - 公式ドキュメント](https://curl.haxx.se/libcurl/)