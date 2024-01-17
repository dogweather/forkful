---
title:                "送信のHTTPリクエスト"
html_title:           "C++: 送信のHTTPリクエスト"
simple_title:         "送信のHTTPリクエスト"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何かとは？
HTTPリクエストを送信することは、ウェブサイトやアプリケーションからデータを取得するためのプロセスです。プログラマーは、必要なデータを取得するためにHTTPリクエストを送信します。

## 方法：
### GETリクエストの送信
```c++
#include <iostream>
#include <curl/curl.h>

// HTTPリクエストを送信する関数
void sendRequest(std::string url){
  CURL *curl;
  CURLcode res;

  // curlの初期化
  curl = curl_easy_init();

  // リクエストの設定
  curl_easy_setopt(curl, CURLOPT_URL, url.c_str());

  // サーバーからのレスポンスをデフォルト出力に出力
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

  // リクエストの送信
  res = curl_easy_perform(curl);

  // エラー処理
  if(res != CURLE_OK)
    std::cout << "Error:" << curl_easy_strerror(res) << std::endl;

  // curlの終了処理
  curl_easy_cleanup(curl);
}

// main関数
int main(){
  std::string url = "https://example.com/api"; // 送信するURL
  sendRequest(url);

  return 0;
}

// curlの出力を受け取り、デフォルト出力に出力する関数
size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    std::cout << (char*)ptr;
    return size * nmemb;
}
```

### POSTリクエストの送信
```c++
#include <iostream>
#include <curl/curl.h>

// HTTPリクエストを送信する関数
void sendRequest(std::string url){
  CURL *curl;
  CURLcode res;

  // curlの初期化
  curl = curl_easy_init();

  // リクエストの設定
  curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "param=value&param2=value2"); // 送信するパラメーターを設定

  // サーバーからのレスポンスをデフォルト出力に出力
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

  // リクエストの送信
  res = curl_easy_perform(curl);

  // エラー処理
  if(res != CURLE_OK)
    std::cout << "Error:" << curl_easy_strerror(res) << std::endl;

  // curlの終了処理
  curl_easy_cleanup(curl);
}

// main関数
int main(){
  std::string url = "https://example.com/api"; // 送信するURL
  sendRequest(url);

  return 0;
}

// curlの出力を受け取り、デフォルト出力に出力する関数
size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    std::cout << (char*)ptr;
    return size * nmemb;
}
```

## 深く掘り下げる：
### 歴史的文脈：
HTTPリクエストは、1990年にTim Berners-Leeによって開発されたウェブの基礎を形作るプロトコルであるHTTP(HyperText Transfer Protocol)の一部です。当初はハイパーテキストドキュメントを転送するために使用されていましたが、次第にウェブ上の様々な要求に応えることができるようになりました。

### 代替案：
HTTPリクエストを送信するためには、上記の例のようにcurlライブラリを使用する方法の他にも、wgetやlibcurlなどのライブラリを使用することもできます。また、特定のプログラミング言語に特化したHTTPリクエストの送信方法もあります。

### 実装の詳細：
HTTPリクエストは、ユーザーが指定したURLにアクセスし、そのサーバーからレスポンスを受け取るというプロセスです。GETリクエストでは、URLに指定したパラメーターを使用してデータを取得し、POSTリクエストでは、指定したパラメーターをサーバーに送信します。また、HTTPリクエストには様々なメソッドがあり、各メソッドによってデータのやり取りの仕方が異なります。

## 関連リンク：
- [curl library](https://curl.haxx.se/)
- [wget](https://www.gnu.org/software/wget/)
- [libcurl](https://curl.haxx.se/libcurl/)
- [HTTP Methods](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)