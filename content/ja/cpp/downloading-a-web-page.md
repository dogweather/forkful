---
title:                "ウェブページをダウンロードする"
html_title:           "C++: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

何をするの？ 
ウェブページのダウンロードとは、インターネット上のウェブページを自分のコンピューターに保存することです。プログラマーがこれを行う理由は、そのページのコンテンツを分析したり、自分のアプリケーションで使用したりするためです。

やり方： 
以下の例では、C++を使用してウェブページをダウンロードする方法を示します。このコードを実行すると、ダウンロードしたウェブページの内容がターミナルに表示されます。 
```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// ウェブページをダウンロードする関数
size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  size_t written;
  written = fwrite(ptr, size, nmemb, stream);
  return written;
}

int main() {
  CURL *curl;
  FILE *fp;
  CURLcode res;

  curl = curl_easy_init();
  if (curl) {
    // ダウンロードするURLを指定
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");
    // ダウンロードしたデータをファイルに書き込む
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    fp = fopen("output.html","wb");
    if (fp) {
      // ダウンロードしたデータをファイルに保存する
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
      // ダウンロードを実行する
      res = curl_easy_perform(curl);
      // 終了処理
      curl_easy_cleanup(curl);
      // ファイルを閉じる
      fclose(fp);
    }
  }
  return 0;
};
```

ディープダイブ： 
ウェブページをダウンロードするプログラムは、1990年代の初めにインターネットが普及し始めた頃から使われています。他の方法としては、ウェブスクレイピングやAPIを使った情報取得もあります。実際のダウンロードでは、インターネット上のウェブページはHTMLと呼ばれる言語で記述されていますが、プログラムではこれを解析して必要な情報を抜き出す必要があります。

参考資料： 
https://curl.se/libcurl/c/