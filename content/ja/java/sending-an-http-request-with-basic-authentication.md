---
title:                "Java: 送信するhttpリクエストと基本認証"
simple_title:         "送信するhttpリクエストと基本認証"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##なぜ
HTTPリクエストを基本認証で送信する理由を1-2文で説明します。

基本認証は、Webサイトやアプリケーションへのアクセス制限を行うために使用されるセキュリティプロトコルです。これにより、ユーザー名とパスワードを使用して認証することができます。

##使い方

```Java
//必要なライブラリのインポート
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Base64;

//基本認証用のユーザー名とパスワード
String username = "username";
String password = "password";

//URLの作成
String urlString = "https://example.com/api";
URL url = new URL(urlString);

//HttpURLConnectionオブジェクトの作成
HttpURLConnection connection = (HttpURLConnection)url.openConnection();
connection.setRequestMethod("GET");

//Basic認証用のユーザー名とパスワードをBase64エンコードしてヘッダーに追加
String auth = username + ":" + password;
String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
connection.setRequestProperty("Authorization", "Basic " + encodedAuth);

//レスポンスコードの取得
int responseCode = connection.getResponseCode();
System.out.println("Response Code: " + responseCode);

//レスポンスの読み込み
BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String inputLine;
StringBuffer response = new StringBuffer();
while ((inputLine = in.readLine()) != null) {
	response.append(inputLine);
}
in.close();

//レスポンスの出力
System.out.println("Response: " + response.toString());
```
上記のコードでは、JavaのHttpURLConnectionクラスを使用して、APIにGETリクエストを送信しています。先に設定したユーザー名とパスワードをBase64エンコードし、認証用のヘッダーに追加することで、基本認証を行っています。

##深堀り

基本認証は、HTTPリクエストのヘッダーにユーザー名とパスワードを含めることで行われます。これにより、サーバー側でユーザー名とパスワードが検証され、アクセスが許可されるかどうかが決められます。

基本認証は、セキュリティレベルが低いため、より安全な認証方式を使用することが推奨されます。また、ユーザー名とパスワードを平文で送信するため、HTTPSを使用して通信を暗号化することも重要です。

##参考文献

- [JavaでHTTPリクエストを送信する方法](https://www.codeflow.site/ja/article/java-http-request)
- [Javaで基本認証を行う方法](https://www.baeldung.com/java-http-request)
- [HTTP Basic認証の仕組み](https://www.infraexpert.com/study/httpauth.html)

##参照
* [認証と認可の違いについて](https://www.atmarkit.co.jp/ait/articles/1203/05/news018.html)