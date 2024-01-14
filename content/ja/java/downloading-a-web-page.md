---
title:                "Java: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

Webページをダウンロードすることの利点はたくさんあります。例えば、オフラインでの閲覧、情報を保存するため、あるいは情報の収集のためなどです。

## 方法

Webページをダウンロードするためには、Javaの「java.net.URL」クラスを使用することができます。以下のコードは、指定したURLのWebページをダウンロードする方法を示しています。

```Java
import java.net.URL;
import java.io.*;

public class WebPageDownloader {
    public static void main(String[] args) throws Exception {
        // URLを指定
        URL url = new URL("https://www.example.com");
        // 接続を開く
        InputStream is = url.openStream();
        // 出力ストリームを開く
        OutputStream os = new FileOutputStream("saved-page.html");

        // ソースをバイト単位で読み込み、ファイルに書き込む
        byte[] b = new byte[2048];
        int length;
        while ((length = is.read(b)) != -1) {
            os.write(b, 0, length);
        }

        // 接続を閉じる
        is.close();
        os.close();
        
        System.out.println("Webページをダウンロードしました。");
    }
}
```

上記のコードを実行すると、指定したURLのWebページが「saved-page.html」という名前のファイルとしてダウンロードされます。

## 詳細

Webページをダウンロードする際には、HTTPプロトコルを使用して通信を行います。HTTPプロトコルには「GET」「POST」などのリクエストメソッドがあり、Webページをダウンロードする場合はGETメソッドが使用されます。また、WebページはHTML言語で記述されており、その構造を解析することで情報を抽出することができます。

## 参考リンク

- [Java URLクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/net/URL.html)
- [HTTPプロトコルとは？](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [HTML言語とは？](https://developer.mozilla.org/ja/docs/Web/HTML)