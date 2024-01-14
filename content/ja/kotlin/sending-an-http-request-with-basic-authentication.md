---
title:                "Kotlin: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストをBasic認証で送信するのか

HTTPリクエストを送信するとき、時にはサーバーによる認証が必要になることがあります。例えば、パスワード保護された情報にアクセスする場合や、セキュリティが重要な操作を行う場合などです。このような場合に、Basic認証を使用することで、ユーザー名とパスワードを用いて簡単に認証を行うことができます。

## 方法

Basic認証を使用してHTTPリクエストを送信するには、Kotlinの`URL`クラスと`HttpURLConnection`クラスを使用します。以下のコード例を参考にしてください。

```Kotlin
val url = URL("https://example.com/protected-info")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
val customAuth = "username:password".toByteArray().encodeBase64()
connection.setRequestProperty("Authorization", "Basic $customAuth")
```

上記のコードでは、まずURLオブジェクトを作成し、そのURLを使用して`openConnection()`メソッドを呼び出します。このメソッドは、`HttpURLConnection`オブジェクトを返します。その後、リクエストメソッドを設定し、ユーザー名とパスワードをBase64エンコードして、リクエストヘッダーに`Authorization`プロパティとして追加します。そして、リクエストを送信する準備が整いました。

上記のコード例では、GETリクエストを使用していますが、必要に応じてリクエストメソッドやボディにデータを追加することができます。

```Kotlin
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "POST"

val postData = "name=John&age=25"
val outputStream = connection.outputStream
outputStream.write(postData.toByteArray(Charsets.UTF_8))
outputStream.close()
```

上記のコードでは、POSTリクエストを使用し、`outputStream`を使用してリクエストボディにデータを追加しています。

## ディープダイブ

Basic認証は、Base64エンコードされた文字列を使用してユーザー名とパスワードを認証するため、非常に安全性が低い方法と言えます。そのため、セキュリティの観点からは推奨されません。代わりに、より安全な認証方法であるDigest認証やOAuth認証を使用することが推奨されます。

また、Basic認証は平文でユーザー名とパスワードが送信されるため、HTTPSの使用を推奨します。これにより、通信内容が暗号化され、セキュリティが向上します。

## 関連リンク

[Basic認証について - MDN](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
[JavaのURLクラスについて - Oracle](https://docs.oracle.com/javase/7/docs/api/java/net/URL.html)
[JavaのHttpURLConnectionクラスについて - Oracle](https://docs.oracle.com/javase/7/docs/api/java/net/HttpURLConnection.html)