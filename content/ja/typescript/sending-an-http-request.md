---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信は、ウェブサーバーに情報をリクエストまたは送信することです。プログラマーがこれを行う理由は、サーバーからデータを取得したり、クライアントから収集したデータをサーバーに送信する必要があるからです。

## どうやって：

```TypeScript
import axios from 'axios';

// Get Request
axios.get('https://example.com/api/resource', {
  headers: {
    'Content-Type': 'application/json'
  }
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.error(error);
});

// Post Request
axios.post('https://example.com/api/resource', {
  data: {
    key: 'value'
  }
}, {
  headers: {
    'Content-Type': 'application/json'
  }
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.error(error);
});
```
このコードは、HTTP GETリクエストとPOSTリクエストを送信する基本的な例です。リクエストのURL،ヘッダ、データ（POSTリクエストの場合）を設定します。

## ディープダイブ：

HTTPリクエストの送信はウェブの基盤です。この概念は、1990年代初頭にWWW（World Wide Web）とともに生まれました。

送信方法として古代の 'XMLHttpRequest' よりも新しく、より効率的な 'Fetch API' がありますが、'axios' はアルゴリズムに対する機能の豊富さや自動変換のために常に人気があります。

具体的な実装について言えば、上記のコード例ではHTTPリクエストを送信するために 'axios' ライブラリを使用しています。これは、ブラウザとNode.jsの両方で動作し、HTTPリクエストの作成を簡単にします。

## 関連情報：

以下に必要なリンクをいくつか示します。

- 'axios' 公式ドキュメンテーション： [https://axios-http.com/](https://axios-http.com/)
- MDN Web Docsの 'Fetch API'： [https://developer.mozilla.org/ja/docs/Web/API/Fetch_API](https://developer.mozilla.org/ja/docs/Web/API/Fetch_API)
- 'XMLHttpRequest' についてのMDN Web Docs： [https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)