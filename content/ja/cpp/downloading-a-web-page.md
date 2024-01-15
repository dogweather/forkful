---
title:                "「Webページのダウンロード」"
html_title:           "C++: 「Webページのダウンロード」"
simple_title:         "「Webページのダウンロード」"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードするアクションに参加する理由は、インターネットを利用する上で必要不可欠なものです。例えば、ウェブサイトのコンテンツをオフラインで閲覧したい場合や、スクレイピングによってデータを収集したい場合など、様々なシーンでダウンロードを行うことがあります。

## 方法

C++を使用してウェブページをダウンロードする方法をご紹介します。下記のコードブロック内に、サンプルのC++コードとその出力結果を記載しています。ご自身のプロジェクトに適用して、ウェブページのダウンロードを実現してみましょう。

```C++
#include <iostream>
#include <curl/curl.h>

int main()
{
    // 初期化
    CURL* curl = curl_easy_init();
    // ダウンロードするURLを設定
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");
    // 出力ファイルを指定
    FILE* fp = fopen("output.html", "wb");
    // 出力先を設定
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    // ダウンロードを実行
    curl_easy_perform(curl);
    // リソースを解放
    curl_easy_cleanup(curl);
    // ファイルをクローズ
    fclose(fp);

    // ダウンロード成功時のメッセージを出力
    std::cout << "ウェブページのダウンロードが完了しました。" << std::endl;

    return 0;
}
```

```
出力結果：
ウェブページのダウンロードが完了しました。
```

## ディープダイブ

ウェブページをダウンロードする際、実際にはHTTPプロトコルを使用してサーバーと通信を行います。C++では、libcurlというライブラリを使用することで簡単にHTTP通信を行うことができます。libcurlを使用することで、ヘッダー情報の設定や認証についても容易に実装することができます。

## 他にも見るべき情報

- [libcurlの公式ドキュメント](https://curl.haxx.se/libcurl/)
- [C++ curlライブラリ 〜基本編〜](https://qiita.com/kurokky/items/8a708d82acfe11caf103)