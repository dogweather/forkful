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

## 何が？ なぜ？

HTTPリクエストを基本認証付きで送信することは、情報を安全にやり取りする上で重要な役割を果たします。プログラマーは、この方法を使用することで、ユーザーの認証やアクセス制限など、アプリケーションのセキュリティを向上させることができます。

## 方法：

Javaでは、HTTPリクエストを基本認証付きで送信するための簡単な方法が用意されています。下記のコードを参考にしてみてください。

```Java
String username = "ユーザー名";
String password = "パスワード";
        
// HTTPリクエストの作成
URL url = new URL("リクエストするURL");
HttpURLConnection connection = (HttpURLConnection)url.openConnection();
connection.setRequestMethod("GET");
connection.setConnectTimeout(5000);
connection.setReadTimeout(5000);
        
// ベーシック認証の設定
String encoded = new String(Base64.getEncoder().encode(String.format("%s:%s", username, password).getBytes()));
connection.setRequestProperty("Authorization", "Basic" + encoded);
        
// レスポンスの取得
int responseCode = connection.getResponseCode();
InputStream stream = null;
if (responseCode == HttpURLConnection.HTTP_OK) {
    stream = connection.getInputStream();
    // 取得したデータの処理
}

```

## 詳細：

HTTPリクエストをベーシック認証付きで送信する方法は、1999年に初めて導入されました。それ以来、基本認証はパスワードを使用したHTTP通信における最も基本的な認証方法として広く使われています。

代替としては、ダイジェスト認証やSSL/TLSなどのより高度な認証方法がありますが、基本認証は簡単な実装と広くサポートされていることから、多くのプログラマーにとって依然として重要な方法です。

実装の詳細については、Javaの公式ドキュメントを参照することができます。

## 関連リンク：

- [Javaの公式ドキュメント](https://docs.oracle.com/javase/tutorial/networking/urls/auth.html)
- [HTTP認証についての詳しい解説](https://d.hatena.ne.jp/tototoshi/20091220/1261334293)