---
date: 2024-01-20 17:44:27.462267-07:00
description: "How to: (\u3084\u308A\u65B9\uFF1A) \u4EE5\u4E0B\u306FJava\u3067Web\u30DA\
  \u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u30B3\u30FC\u30C9\
  \u4F8B\u3067\u3059\u3002\u5FC5\u8981\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\
  \u30F3\u30DD\u30FC\u30C8\u3057\u3066URL\u304B\u3089\u30B3\u30F3\u30C6\u30F3\u30C4\
  \u3092\u8AAD\u307F\u8FBC\u3080\u7C21\u5358\u306A\u624B\u9806\u3092\u7528\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.945392-06:00'
model: gpt-4-1106-preview
summary: "\u4EE5\u4E0B\u306FJava\u3067Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\
  \u30ED\u30FC\u30C9\u3059\u308B\u30B3\u30FC\u30C9\u4F8B\u3067\u3059\u3002\u5FC5\u8981\
  \u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30DD\u30FC\u30C8\u3057\u3066\
  URL\u304B\u3089\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u8AAD\u307F\u8FBC\u3080\u7C21\
  \u5358\u306A\u624B\u9806\u3092\u7528\u3044\u307E\u3059."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

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
