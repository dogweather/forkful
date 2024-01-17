---
title:                "HTTPリクエストの送信"
html_title:           "Java: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 何をしているのか？ 何故？
HTTPリクエストを送ることは、ウェブ上で情報をやりとりするための方法です。プログラマーがそれを行う理由は、例えばウェブサイトから情報を取得したり、データを送信したりするためです。

# やり方：
下記は、JavaでHTTPリクエストを送る方法の例です。コードの実行結果は、 ` // ` の後に記載されています。

```Java
// JavaでHTTPリクエストを送る例：
URL url = new URL("https://www.example.com/"); // リクエスト先のURLを指定
HttpURLConnection con = (HttpURLConnection) url.openConnection(); // 接続を開く
con.setRequestMethod("GET"); // リクエストメソッドを指定
int responseCode = con.getResponseCode(); // レスポンスコードを取得
System.out.println("レスポンスコード: " + responseCode); // レスポンスコードを表示
con.disconnect(); // 接続を閉じる
```

```
// 実行結果：
レスポンスコード: 200
```

# 詳細：
HTTPリクエストの問題については、Webの発展と共に技術も発展してきました。HTTP以外のプロトコルもあるため、リクエストを送る方法として、他の方法もあります。また、プログラマーがHTTPリクエストを行うためには、ネットワーク接続や認証などの詳細を考慮する必要があります。

# 関連リンク：
- [JavaでHTTPリクエストを実行する](https://stackoverflow.com/questions/1359689/how-to-send-http-request-in-java)
- [HTTPリクエストの種類](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)
- [Java HTTPクライアント API](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)