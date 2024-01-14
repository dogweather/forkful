---
title:                "TypeScript: 「httpリクエストの送信」"
simple_title:         "「httpリクエストの送信」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ？

何かをするときにHTTPリクエストを送信することは、Web開発の重要な側面です。これにより、Webブラウザやサーバー、データベースなどのリソースとのコミュニケーションが可能になります。しかし、多くの初心者開発者は、HTTPリクエストを正しく理解せずにコーディングを始めてしまうことがあるかもしれません。この記事では、TypeScriptを使ってHTTPリクエストを送信する方法を紹介します。

## 方法

まずは、HTTPリクエストを送信するために必要な基本的なコードを見てみましょう。

```TypeScript
import axios from 'axios';

axios.get('https://example.com/api/users')
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.log(error);
  });
```

簡単ですね！このコードでは、axiosというライブラリを使って`https://example.com/api/users`というURLにGETリクエストを送信しています。そして、サーバーからのレスポンスが`response`に格納され、その中の`data`をコンソールに出力します。もしエラーが発生した場合は、`catch`の中で処理を行います。

次に、POSTリクエストを送信する例を見てみましょう。

```TypeScript
import axios from 'axios';

const data = {
  name: 'John',
  age: 30
};

axios.post('https://example.com/api/users', data)
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.log(error);
  });
```

このコードでは、`data`というオブジェクトを作成し、それを`axios.post`の第二引数に渡しています。これにより、サーバーにデータを送信することができます。

また、HTTPリクエストには、さまざまなオプションを設定することができます。例えば、ヘッダーやタイムアウトの設定などです。詳細については、公式ドキュメントを参照してください。

## 深堀り

この記事では、axiosというライブラリを使用してHTTPリクエストを送信しましたが、実際にはさまざまな方法でHTTPリクエストをすることができます。例えば、fetchやXMLHttpRequestなどのネイティブのAPIを使うこともできます。

また、RESTful APIを使う場合は、GETやPOST以外にもPUTやDELETEなどのメソッドを使うこともあります。それぞれのメソッドには、どのような目的があるのか、どのように使うのかを理解することが重要です。

もし開発中に問題が発生した場合は、デベロッパーツールやネットワークタブを使って、実際にリクエストとレスポンスを確認してみることをおすすめします。これにより、リクエストやレスポンスの内容を正しく理解できるようになります。

## 併せて参照

- [axios 公式ドキュメント](https://axios-http.com/docs/intro)
- [Fetch API ドキュメント](https://developer.mozilla.org/ja/docs/Web/API/Fetch_API/Using_Fetch)
- [XMLHttpRequest ドキュメント](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)