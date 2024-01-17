---
title:                "基本認証付きのhttpリクエストの送信"
html_title:           "C#: 基本認証付きのhttpリクエストの送信"
simple_title:         "基本認証付きのhttpリクエストの送信"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何？なぜ？

HTTP要求を基本認証で送信することは、プログラマーがWebサービスに接続するための一般的な方法です。この方法は、セキュリティを強化し、機密性の高い情報を保護するために使用されます。

## 方法：

```C#
// リクエストを作成
var request = (HttpWebRequest)WebRequest.Create("https://example.com/api/endpoint");
request.Method = "GET";

// 基本認証ヘッダーを作成
string credentials = "username:password";
Byte[] credentialBytes = Encoding.UTF8.GetBytes(credentials);
string encodedCredentials = Convert.ToBase64String(credentialBytes);
request.Headers.Add("Authorization", "Basic " + encodedCredentials);

// レスポンスを取得
var response = (HttpWebResponse)request.GetResponse();
using (var streamReader = new StreamReader(response.GetResponseStream()))
{
    var result = streamReader.ReadToEnd();
    Console.WriteLine(result);  // サーバーからの応答を表示
}
```

## ディープダイブ：

基本認証は、HTTP仕様の一部であり、長年にわたって使用されてきた古典的な認証方法です。近年、より安全な認証方法が開発されているため、基本認証は使用されなくなる可能性があります。代替手段として、トークンベースの認証があります。

基本認証を実装するには、リクエストヘッダーにHTTP Authorizationヘッダーの形式でユーザー名とパスワードを含める必要があります。また、セキュリティを強化するために、HTTPS通信を使用することをお勧めします。

## 関連情報：

- [HTTP Basic Access Authentication](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_Authentication_scheme)
- [HTTP Authentication](https://www.w3.org/Protocols/HTTP/Authentication.html)
- [Token-based authentication vs. session-based authentication](https://medium.com/@sherryhsu/session-vs-token-based-authentication-11a6c5ac45e4)