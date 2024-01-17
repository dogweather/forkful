---
title:                "HTTPリクエストを送信する"
html_title:           "TypeScript: HTTPリクエストを送信する"
simple_title:         "HTTPリクエストを送信する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

「何となぜ？」

HTTPリクエストとは、ウェブサーバー上でデータを要求するためのものです。プログラマーがHTTPリクエストをする理由は、例えばデータベースから情報を取得するためや、外部APIとの通信を行うためです。

「方法：」

TypeScriptでHTTPリクエストをする具体的な例を示します。まずは、requestライブラリをインストールします。

```TypeScript
npm install request
```

次に、リクエストをするための基本的なコードを書きます。

```TypeScript
let request = require('request');

request('https://example.com', function (error, response, body) {
  if (!error && response.statusCode == 200) {
    console.log(body);
  }
});
```

このコードでは、requestライブラリを使い、指定したURLにリクエストを送り、レスポンスを取得しています。レスポンスが成功した場合には、取得したデータをコンソールに出力しています。

「深堀り：」

HTTPリクエストは、1991年に最初に導入されました。他にも、jQueryやAxiosなどのライブラリを使ってリクエストを行うこともできます。また、ブラウザ上でリクエストを行うこともできますが、セキュリティの観点から推奨されません。

「関連情報：」

- [requestライブラリ](https://www.npmjs.com/package/request)
- [jQueryライブラリ](https://jquery.com/)
- [Axiosライブラリ](https://www.npmjs.com/package/axios)