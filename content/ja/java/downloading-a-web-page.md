---
title:                "ウェブページのダウンロード"
html_title:           "Java: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由は、そのコンテンツを読んだり、取得したり、保存したりするためです。

## ダウンロードする方法

ダウンロードするには、Javaの標準ライブラリであるjava.netパッケージのURLConnectionクラスを使用します。以下のコードを使用して、指定したURLからウェブページをダウンロードし、コンソールに出力することができます。

```Java
import java.net.*;
import java.io.*;

public class DownloadWebPage{
  public static void main (String[] args) {
    try {
      URL url = new URL("https://example.com"); // ダウンロードするウェブページのURL
      URLConnection connection = url.openConnection();
      BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      String inputLine;
      while ((inputLine = in.readLine()) != null) {
        System.out.println(inputLine); // ウェブページのコンテンツを一行ずつ出力
      }
      in.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

上記のコードを実行すると、例えば「Hello, world!」を含むウェブページの場合、コンソールには次のように出力されます。

```
<!DOCTYPE html>
<html>
<head>
<title>Hello, world!</title>
<meta charset="UTF-8">
</head>
<body>
<h1>Hello, world!</h1>
</body>
</html>
```

## 深堀り

URLConnectionクラスは、URLからデータを取得するための機能を提供します。上記の例では、getInputStream()メソッドを使用してウェブページのコンテンツを取得しています。また、URLを使用して画像や動画などのメディアファイルをダウンロードすることも可能です。URLConnectionクラスは、HTTPやFTPなどのプロトコルをサポートしており、様々な種類のデータを取得することができます。

## 参考リンク

- [URLConnectionクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/net/URLConnection.html)
- [Javaでwebページをダウンロードする方法](https://www.baeldung.com/java-download-webpage)