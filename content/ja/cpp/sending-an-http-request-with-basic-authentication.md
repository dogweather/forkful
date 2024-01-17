---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C++: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何？なぜ？
HTTPリクエストを基本認証付きで送るとは何かと、プログラマーがそれを行う理由について2-3文で説明します。

## 方法：
以下のような、Ｃ＋＋コードブロック内のコーディング例やサンプル出力を使用して説明します。
```
// ライブラリの読み込み
#include <iostream>
#include <curl/curl.h>

// リクエスト用の構造体を定義
struct MemoryStruct {
  char *memory;
  size_t size;
};

// リクエスト用のコールバック関数を定義
static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)userp;

  mem->memory = (char *)realloc(mem->memory, mem->size + realsize + 1);
  if (mem->memory == NULL) {
    /* out of memory! */
    std::cout << "not enough memory (realloc returned NULL)\n";
    return 0;
  }

  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;

  return realsize;
}

int main(void) {
  // 変数の初期化
  CURL *curl_handle;
  CURLcode res;
  struct MemoryStruct chunk;
  chunk.memory = (char *)malloc(1);  /* will be grown as needed by the realloc above */
  chunk.size = 0;    /* no data at this point */

  // cURLの初期化
  curl_global_init(CURL_GLOBAL_ALL);

  // cURLのハンドルを取得
  curl_handle = curl_easy_init();

  // URLを指定
  curl_easy_setopt(curl_handle, CURLOPT_URL, "https://example.com");

  // 基本認証を使用するためのオプションを設定
  curl_easy_setopt(curl_handle, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
  curl_easy_setopt(curl_handle, CURLOPT_USERPWD, "username:password");

  // リクエストを送信し、結果を取得
  curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
  curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *)&chunk);
  res = curl_easy_perform(curl_handle);

  // 送信結果を出力
  if (res != CURLE_OK) {
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
  }
  else {
    printf("%lu bytes retrieved\n", (long)chunk.size);
    printf("%s\n", chunk.memory);
  }

  // ハンドルを解放
  curl_easy_cleanup(curl_handle);

  // メモリを解放
  free(chunk.memory);

  // cURLを終了
  curl_global_cleanup();

  return 0;
}
```

## もっと深く：
基本認証を使用してHTTPリクエストを送ることは、コンピュータネットワークの歴史と深く関わっています。プログラマーが基本認証を選択する理由としては、シンプルで簡単に実装できることや、ユーザー名とパスワードの組み合わせでのセキュリティが提供されることが挙げられます。代替手段としては、OAuthやOpenIDなどのより安全な認証方式がありますが、基本認証は依然としてメジャーな選択肢です。

## 関連情報：
基本認証を使用したHTTPリクエストの詳細については、以下のリンクを参照してください。

- curlライブラリ：https://curl.haxx.se/
- cURLライブラリのドキュメント：https://curl.haxx.se/libcurl/
- cURLの基本認証に関するドキュメント：https://curl.haxx.se/libcurl/c/CURLOPT_HTTPAUTH.html