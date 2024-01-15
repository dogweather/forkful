---
title:                "「HTTPリクエストの送信」"
html_title:           "Java: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

なぜ私たちがHTTPリクエストを送信するのか？それは、ウェブサイトやアプリケーションから必要な情報を取得したり、データを送信したりするためです。HTTPリクエストは、インターネット上でデータをやりとりするために非常に重要です。

## 方法

```Java
// HTTPリクエストを送信するためには、HttpURLConnectionクラスを使用します。
URL url = new URL("https://example.com"); // リクエストを送信する先のURLを指定します
HttpURLConnection con = (HttpURLConnection) url.openConnection(); // URLへの接続を確立します
con.setRequestMethod("GET"); // GETリクエストを送信するように設定します
// レスポンスコードが200の場合、リクエストが成功したことを意味します
if (con.getResponseCode() == 200) {
    // リクエストからレスポンスデータを取得します
    BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
    String inputLine;
    StringBuffer content = new StringBuffer();
    while ((inputLine = in.readLine()) != null) {
        // レスポンスデータを読み込んで文字列として追加します
        content.append(inputLine);
    }
    in.close();
    System.out.println(content.toString()); // レスポンスデータを表示します
}
con.disconnect(); // 接続を切断します
```

## 深堀り

HTTPリクエストは、ウェブサイトやアプリケーションでデータの送受信を行うためのプロトコルです。HTTPリクエストは、URLを使用して接続先のサーバーを指定し、リクエストの種類（GET、POST、PUTなど）やヘッダー、ボディなどの情報を付加して送信します。サーバー側では、受け取ったリクエストに応じて適切な処理を行い、レスポンスを返します。HTTPリクエストを簡単に実装するためには、JavaにはHttpURLConnectionクラスが用意されています。詳しい使用方法や異なるタイプのリクエストの実装などは、ドキュメントやそれぞれのAPIの仕様を確認することで理解することができます。

## 関連リンク

- [Java SEドキュメント](https://docs.oracle.com/javase/jp/)
- [HttpURLConnectionクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/net/HttpURLConnection.html)
- [HTTPリクエストとは？](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)