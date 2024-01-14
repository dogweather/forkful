---
title:                "Javascript: 「HTTP リクエストを送信する」"
simple_title:         "「HTTP リクエストを送信する」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信することのメリットは何でしょうか？それを知るには、最初にHTTPを理解する必要があります。HTTPとは、ハイパーテキストトランスファープロトコルの略で、ウェブサーバーとクライアント間の通信を処理するために使用されます。つまり、HTTPリクエストを送信することで、ウェブサイトやアプリケーションでデータをやり取りすることができるのです。

## 送信する方法

HTTPリクエストを送信するためには、Javascriptの標準的な方法である`XMLHttpRequest`（XHR）オブジェクトを使用します。例えば、以下のようなコードを書くことで、GETリクエストを送信し、サーバーからのレスポンスを取得することができます。

```Javascript
let xhr = new XMLHttpRequest();
xhr.open('GET', 'https://example.com/api/users');
xhr.send();

xhr.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(xhr.responseText);
  }
};
```

上記の例では、`XMLHttpRequest`オブジェクトの`open`メソッドで、リクエストのタイプ（ここではGET）、URLを指定し、`send`メソッドでリクエストを送信します。そして、`onreadystatechange`イベントで、リクエストの状態が完了（`readyState == 4`）かつステータスが成功（`status == 200`）の場合に、サーバーからのレスポンスを取得します。

## より詳細な情報

HTTPリクエストを送信する際、さまざまなタイプのリクエストを使用することができます。ここでは、最も一般的なGETリクエストの例を紹介しましたが、PUT、POST、DELETEなどのリクエストも同じように送信できます。また、リクエストのパラメーターやヘッダーを指定することも可能です。

また、リクエストを送信する際には、エラーハンドリングも重要です。必ずリクエストが成功したかどうかを確認し、エラーメッセージを処理するようにしましょう。

## 参考リンク

- [HTTPリクエストとは？基本から詳しく解説](https://www.sejuku.net/blog/26459)
- [XMLHttpRequest - MDN](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)
- [コンテンツリクエストを行う - APIリファレンス](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/URLLoadingSystem/Articles/UsingNSURLSession.html#//apple_ref/doc/uid/TP40013509-SW1)

## 関連リンクを見る