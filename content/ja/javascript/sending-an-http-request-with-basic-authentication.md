---
title:                "基本身分認証でのhttpリクエストの送信"
html_title:           "Javascript: 基本身分認証でのhttpリクエストの送信"
simple_title:         "基本身分認証でのhttpリクエストの送信"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
今回の記事では、JavaScriptでHTTPリクエストを送信する際に、基本認証を使用する理由について説明します。また、基本認証を実装する方法と、その仕組みをより詳しく知るための情報をご紹介します。

## 実際にやってみる
基本認証を使用してHTTPリクエストを送信するには、まずは`XMLHttpRequest`オブジェクトを使用します。以下のコードを参考にしてみてください。

```Javascript
// XMLHttpRequestオブジェクトを作成
var xhttp = new XMLHttpRequest();

// リクエストの種類とURLを指定
xhttp.open("GET", "https://example.com/api/data", true);

// ユーザ名とパスワードを指定して基本認証を設定
xhttp.setRequestHeader("Authorization", "Basic " + btoa("username:password"));

// リクエストを送信
xhttp.send();

// レスポンスを受け取る
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(xhttp.responseText);
  }
};
```

上記のコードでは、`XMLHttpRequest`オブジェクトを作成し、`open()`メソッドでリクエストの種類とURLを指定しています。さらに、`setRequestHeader()`メソッドでユーザ名とパスワードをBase64エンコードしたものを`Authorization`ヘッダーに設定することで、基本認証を実装しています。最後に`send()`メソッドでリクエストを送信し、`onreadystatechange`イベントハンドラーを使用してレスポンスを受け取ることができます。

`XMLHttpRequest`以外にも`fetch()`APIやサードパーティー製のHTTPクライアントライブラリを使用しても、同様の方法で基本認証を実装することができます。

## より詳しく見てみる
基本認証は、HTTPプロトコルの一種であるHTTPベーシック認証を使用するものです。これは、リクエストヘッダーにBase64エンコードされたユーザ名とパスワードを含めることで、サーバー側で認証を行う仕組みです。ただし、Base64エンコードは暗号化ではないため、セキュリティ上の問題があることには注意が必要です。

実際のアプリケーションでは、HTTPSを使用することや、セキュリティトークンを使用することで、より安全な認証を実現することが推奨されています。

## 関連リンク
- [MDN web docs - HTTPベーシック認証](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [XMLHttpRequestオブジェクトのリファレンス](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)
- [Base64エンコードについて](https://developer.mozilla.org/ja/docs/Web/API/DOMString/Btoa)

### おわりに
今回は、JavaScriptでHTTPリクエストを送信する際に、基本認証を使用する方法について紹介しました。基本認証は便利な認証方式ですが、セキュリティ上の問題があるため、適切に使用するように注意しましょう。

## 関連記事を見る