---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？
HTTPリクエストの送信は、Webサーバーに対する指示です。これを行うと、APIからデータを取得したり、Webサービスと対話したりすることができます。

## どうやって：
以下に、JavaでHTTP requestを送信する一例を示します：

```Java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
import java.net.http.HttpHeaders;
import java.net.http.HttpClient.Version;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newBuilder().version(Version.HTTP_2).build();
        HttpRequest request = HttpRequest.newBuilder().uri(URI.create("http://example.com")).GET().build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        
        System.out.println("Response code is: " + response.statusCode());
        HttpHeaders headers = response.headers();
        headers.map().forEach((k, v) -> System.out.println(k + ":" + v));
    }
}
```

このコードは、"http://example.com"へのHTTP GETリクエストを作成し、送信してから、レスポンスを表示します。

## 詳細掘り下げ：
HTTPリクエストの送信は、インターネットの基本的な機能です。初期のインターネットはNCSA Mosaicなどのブラウザを通じて人々に開放され、HTTPリクエストとレスポンスの概念が広く知られることになりました。JavaでHTTPリクエストを送信するための別の方法は、Apache HttpClientなどのサードパーティのライブラリを使用することですが、Java 11以降では、Java標準ライブラリのHttpClientがあります。'GET','POST','PUT','DELETE'などの様々なHTTPメソッドをサポートし、レスポンスのハンドリングも柔軟に行うことができます。

## 参照：
- [OracleのJava 11 HttpClientガイド](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/)