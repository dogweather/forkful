---
title:                "ウェブページのダウンロード"
html_title:           "C: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ
人々がWebページをダウンロードするのに理由を最大限に2つの文で説明する。

人々はインターネット上の情報にアクセスしたいと考えることがあります。 しかし、オフラインでその情報にアクセスしたい場合、Webページをダウンロードして保存する必要があります。 例えば、電子書籍やオンライン記事の保存などです。 C言語を使えば、簡単にWebページをダウンロードすることができます。

## 使い方
WebページをダウンロードするためのC言語のコード例を紹介します。 まず、必要なライブラリをインクルードします。
    
    ```C
    #include <stdio.h>
    #include <stdlib.h>
    #include <curl/curl.h>
```

次に、`CURL`ライブラリを使ってWebページをダウンロードします。

```C
CURL *curl;
CURLcode res;
FILE *fp;

// ダウンロードするWebページのURLを指定
char* url = "https://www.example.com/";

// ダウンロードしたページを保存するファイル名を指定
char outfilename[FILENAME_MAX] = "output.html";

// CURLセッションを初期化
curl = curl_easy_init();

if(curl) {
  // ダウンロードしたページをファイルに保存
  fp = fopen(outfilename,"wb");
  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
  res = curl_easy_perform(curl);
  // エラーチェック
  if(res != CURLE_OK)
    fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
  // セッションを閉じる
  curl_easy_cleanup(curl);
  // ファイルを閉じる
  fclose(fp);
}
```

実行すると、指定したURLからWebページがダウンロードされ、指定したファイルに保存されます。 ファイルが正しくダウンロードされたかを確認するために、以下のようなコードを追加しても良いでしょう。

```C
// ダウンロードしたページを表示する
fp = fopen(outfilename, "rb");
if (fp) {
  char buf[4096];
  while(!feof(fp)) {
    size_t n = fread(buf, 1, sizeof(buf), fp);
    fwrite(buf, 1, n, stdout);
  }
  fclose(fp);
}
```

## 深堀り
C言語を使ってWebページをダウンロードする方法について深く掘り下げてみましょう。

まず、`CURL`ライブラリが何であるかから説明しましょう。 `CURL`とは、URLを使ってファイルをダウンロードするためのライブラリであり、HTTP、FTP、HTTPS、SMTPなどのプロトコルに対応しています。

次に、先ほど紹介したコードについて詳しく説明します。 コードの中で`curl_easy_setopt()`関数を使って、様々なオプションを指定しています。 例えば、`CURLOPT_URL`オプションでダウンロードするWebページのURLを指定し、`CURLOPT_WRITEFUNCTION`オプションでダウンロードしたページをどのように保存するかを指定しています。 `CURLOPT_WRITEFUNCTION`オプションでは、データを受け取るためのコールバック関数を指定することができます。

さらに、`curl