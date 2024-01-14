---
title:                "C#: 基本認証を使用してHTTPリクエストを送信する"
simple_title:         "基本認証を使用してHTTPリクエストを送信する"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# なぜHTTPリクエストを基本認証付きで送信するのか？
 
HTTPリクエストを送信する際には、サーバー側でリクエストを認証する必要があります。基本認証は、ユーザー名とパスワードを使用してリクエストを認証する一般的な方法です。これにより、セキュリティを確保し、権限を持たないユーザーからのアクセスを制限することができます。基本認証を使用することで、安全で信頼性の高いHTTPリクエストを送信することができます。
 
## 方法
 
基本認証付きのHTTPリクエストを送信するには、まずリクエストを送信するためのURLを指定します。次に、`WebRequest`クラスを使用してリクエストオブジェクトを作成し、`Credentials`プロパティを使用してユーザー名とパスワードを指定します。最後に、作成したリクエストオブジェクトを使用して`GetResponse`メソッドを呼び出し、サーバーからの応答を取得します。以下のコード例を参考にしてください。
 
```C#
// リクエストを送信するURLを指定
string url = "https://example.com/api";

// リクエストオブジェクトを作成
WebRequest request = WebRequest.Create(url);

// ユーザー名とパスワードを指定
request.Credentials = new NetworkCredential("ユーザー名", "パスワード");

// リクエストを送信し、サーバーからの応答を取得
WebResponse response = request.GetResponse();

// 応答を文字列として取得
string responseString = new StreamReader(response.GetResponseStream()).ReadToEnd();

// 出力
Console.WriteLine(responseString);
// -> サーバーからの応答が出力される
```
 
## 深堀り
 
基本認証は、HTTPヘッダーの`Authentication`フィールドを使用して行われます。このフィールドには、`Basic`というプロトコル名と、ユーザー名とパスワードをBase64エンコードした文字列が含まれます。サーバー側では、この情報を使用してユーザーを認証し、リクエストを受け付けるかどうかを決定します。
 
また、基本認証は安全性が低いため、HTTPSと組み合わせて使用することが推奨されます。HTTPSを使用することで、リクエストとレスポンスの間の通信が暗号化され、セキュリティを高めることができます。
 
## さらに読む
 
- [C#でHTTPSリクエストを送信する方法](https://example.com/https-request-csharp)
- [Basic認証の仕組みについて](https://example.com/basic-authentication-info)