---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:27.462267-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
Webページをダウンロードするとは、インターネット上のページ内容をプログラムで取得することです。プログラマーがこれをする理由は、データの収集、解析、オフラインでの閲覧、またはその内容を他のアプリケーションで使用するためです。

## How to: (やり方：)
以下はJavaでWebページをダウンロードするコード例です。必要なライブラリをインポートしてURLからコンテンツを読み込む簡単な手順を用います。

```Java
import java.io.*;
import java.net.URL;

public class WebPageDownloader {

    public static void main(String[] args) {
        String webPageUrl = "http://example.com";
        String fileName = "downloaded_page.html";
        
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new URL(webPageUrl).openStream()));
             BufferedWriter writer = new BufferedWriter(new FileWriter(fileName))) {
            
            String line;
            while ((line = reader.readLine()) != null) {
                writer.write(line);
                writer.newLine();
            }
            System.out.println("Web page downloaded to " + fileName);
        } catch (IOException e) {
            System.err.println("An error occurred: " + e.getMessage());
        }
    }
}
```

実行結果：
```
Web page downloaded to downloaded_page.html
```

## Deep Dive (詳細解説)
ダウンロードしたWebページは初めてHTMLが誕生した1990年代から変わらぬ基本ですが、方法は進化しました。初期は単純なHTTPリクエストでした。現在では、Javaでは`HttpURLConnection`やApacheの`HttpClient`、さらにはJsoupのようなライブラリを使用してHTMLの解析まで行うことが可能です。

`URLConnection`はJavaの標準ライブラリで提供されており、シンプルな使い方は上の例に示す通りです。しかし、`URLConnection`は古めのAPIであり、使いにくい面もあります。そのため、より現代的かつ機能的な`HttpClient`をJava 11から標準で使用することができます。

Jsoupは、HTMLをパースしてDOM操作を可能にする強力なライブラリです。Webスクレイピングでよく使われます。Jsoupではページのダウンロードと処理を一行で行えるので、高度な処理が必要な場合に非常に便利です。

## See Also (参考資料)
- Java `URLConnection`: https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html
- Java `HttpClient`: https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html
- Jsoup: https://jsoup.org/
- HTTPリクエストとは: https://developer.mozilla.org/ja/docs/Web/HTTP/Methods