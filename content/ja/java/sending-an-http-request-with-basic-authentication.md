---
title:                "ベーシック認証を使用してhttpリクエストを送信する"
html_title:           "Java: ベーシック認証を使用してhttpリクエストを送信する"
simple_title:         "ベーシック認証を使用してhttpリクエストを送信する"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
なぜ、HTTPリクエストで基本認証を使用するのでしょうか？HTTPリクエストを行う際には、サーバーに対して認証を行う必要があるためです。これにより、セキュリティを確保し、権限のないアクセスを防止することができます。

## 方法
以下のように、Javaを使用してHTTPリクエストに基本認証情報を含めることができます。

```Java
// リクエスト用のURLを作成
URL url = new URL("http://www.example.com/api");

// HTTPコネクションを作成
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// リクエストメソッドを設定
connection.setRequestMethod("GET");

// 認証情報をヘッダーに設定
String username = "ユーザー名";
String password = "パスワード";
String authStr = username + ":" + password;
String encodedAuthStr = Base64.getEncoder().encodeToString(authStr.getBytes());
connection.setRequestProperty("Authorization", "Basic " + encodedAuthStr);

// レスポンスコードを取得
int responseCode = connection.getResponseCode();

// レスポンスを取得
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
StringBuffer response = new StringBuffer();
while ((inputLine = reader.readLine()) != null) {
    response.append(inputLine);
}
reader.close();

// レスポンスを出力
System.out.println(response.toString());
```

**出力：**
```
Hello World!
```

## 深堀り
基本認証とは、ユーザー名とパスワードを使用して認証を行う方法です。HTTPリクエストにおいては、リクエストヘッダーにAuthorizationフィールドを追加し、その値として "Basic" と認証情報をBase64エンコードした文字列を設定することで、認証を行うことができます。

## 関連リンク
- [Javaのベース64エンコード](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [HTTPの基本認証](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [Java HttpURLConnectionクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/net/HttpURLConnection.html)