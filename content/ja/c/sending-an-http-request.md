---
title:                "「HTTPリクエストの送信」"
html_title:           "C: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何をして、何のために？

HTTPリクエストを送ることは、Webサーバーやクライアント間でデータをやり取りする方法です。プログラマーは、この方法を使うことで、Webサービスを作成し、データを取得したり送信したりすることができます。

## 方法：

以下の例を参考に、```C ... ```コードブロック内にコーディング例とサンプルの出力を記述します。

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  return 0;
}
```

### 出力：

```
curl_easy_perform() failed: Couldn't connect to server
```

## 深堀り：

- **歴史的背景：** HTTPリクエストは、1996年に最初のバージョンが公開されました。その後、HTTP/1.1と呼ばれるバージョンが定着しましたが、最近ではHTTP/2が普及してきています。
- **代替方法：** HTTPリクエストを送る方法には、他のプログラミング言語やツールもあります。例えば、PythonのRequestsライブラリやPostmanというツールなどがあります。
- **実装の詳細：** HTTPリクエストには、GETやPOSTのようなさまざまなメソッドがあります。また、リクエストヘッダーやボディーなどのパラメーターを指定することで、より詳細なリクエストを行うことができます。

## 関連情報：

- [curl - man page](https://curl.haxx.se/docs/manpage.html)
- [HTTP/2の仕様書](https://tools.ietf.org/html/rfc7540)
- [Python Requestsライブラリの公式ドキュメント](https://2.python-requests.org/en/master/)
- [Postman公式サイト](https://www.postman.com/)