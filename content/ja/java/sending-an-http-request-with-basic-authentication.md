---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何と何故？
HTTPリクエストのベーシック認証の送信は、ユーザー名とパスワードを持つクライアントがサーバーに対してアクセス許可を要求するプロセスです。これは、プログラマが信頼できる通信を定立するために行います。

## 手順：
```Java
// Java HTTP Clientライブラリを用いた例示的なコード

import java.net.Authenticator;
import java.net.http.HttpClient;
import java.net.PasswordAuthentication;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newBuilder().authenticator(new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication("username", "password".toCharArray());
            }
        }).build();

        HttpRequest request = HttpRequest.newBuilder().uri(new URI("http://example.com")).build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```
上記のコードで、HTTPクライアントがベーシック認証を用いてexample.comにリクエストを送信します。レスポンスの本体がコンソールに表示されます。

## 深層探索
1. **歴史的背景**: HTTP Basic認証はHTTPプロトコルの初期段階から存在しています。しかし、パスワードを平文で送信する問題があるため、現在はHTTPSやOAuthなどのより安全な手段が主流になっています。
2. **代替手段**: 認証方式は多岐にわたり、ベーシック認証の代わりにDigest認証、Bearer認証（OAuth、JWT等）、Mutual認証等が存在します。
3. **実装詳細**: Java HTTP Clientライブラリを使用すると、ベーシック認証の設定はAuthenticatorクラスを介して簡単に行うことができます。これはユーザ名とパスワードをエンコードし、その結果を"Authorization"ヘッダに追加します。

## 参考リンク
1. より深い理解のためのJavaSE公式ドキュメント [HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
2. JavaでのBasic認証詳細 [Oracle tutorial](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
3. Secure Communication with HTTPS [Oracle guide](https://docs.oracle.com/en/java/javase/11/security/java-secure-socket-extension-jsse-reference-guide.html#GUID-426A14C7-4C22-45F5-9B5C-32C6AF5B5964)