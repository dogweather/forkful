---
date: 2024-01-20 17:44:27.462267-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30DA\
  \u30FC\u30B8\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u53D6\u5F97\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\
  \u308C\u3092\u3059\u308B\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u306E\u53CE\u96C6\
  \u3001\u89E3\u6790\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\u3067\u306E\u95B2\u89A7\u3001\
  \u307E\u305F\u306F\u305D\u306E\u5185\u5BB9\u3092\u4ED6\u306E\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u3067\u4F7F\u7528\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.945392-06:00'
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30DA\u30FC\
  \u30B8\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\
  \u3092\u3059\u308B\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u306E\u53CE\u96C6\u3001\
  \u89E3\u6790\u3001\u30AA\u30D5\u30E9\u30A4\u30F3\u3067\u306E\u95B2\u89A7\u3001\u307E\
  \u305F\u306F\u305D\u306E\u5185\u5BB9\u3092\u4ED6\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u3067\u4F7F\u7528\u3059\u308B\u305F\u3081\u3067\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
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
