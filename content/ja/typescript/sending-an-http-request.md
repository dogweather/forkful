---
title:                "HTTPリクエストの送信"
html_title:           "TypeScript: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

人々がHTTPリクエストを送信する理由を最大2文で説明すると、それは外部のAPIやデータベースからデータを取得したり、ウェブサービスとの通信を行うためです。

## 使い方

**HTTPリクエストを送信する前に必要な準備**

1. `axios`パッケージをインストールします。

   ```TypeScript
   npm install axios
   ```

2. `axios`をインポートします。

   ```TypeScript
   import axios from 'axios';
   ```

**GETリクエストを送信する方法**

```TypeScript
axios.get('https://jsonplaceholder.typicode.com/posts')
  .then((response) => console.log(response))
  .catch((error) => console.log(error));
```

**POSTリクエストを送信する方法**

```TypeScript
const data = {
  title: 'My New Post',
  body: 'This is a new post about TypeScript.',
  userId: 1,
};

axios.post('https://jsonplaceholder.typicode.com/posts', data)
  .then((response) => console.log(response))
  .catch((error) => console.log(error));
```

**PUTリクエストを送信する方法**

```TypeScript
const data = {
  title: 'Updated Post',
  body: 'This post has been updated.',
};

axios.put('https://jsonplaceholder.typicode.com/posts/1', data)
  .then((response) => console.log(response))
  .catch((error) => console.log(error));
```

**DELETEリクエストを送信する方法**

```TypeScript
axios.delete('https://jsonplaceholder.typicode.com/posts/1')
  .then((response) => console.log(response))
  .catch((error) => console.log(error));
```

## 詳細を掘り下げる

HTTPリクエストを送信する際、`axios`を使用することで非同期処理を簡単に行えます。また、`catch`メソッドを使用することでエラーハンドリングを行うことができます。さらに、レスポンスデータを取得するには`then`メソッドを使用し、エラーを表示するには`catch`メソッドを使用します。

## 関連リンク

[axios公式ドキュメント](https://axios-http.com/)
[MDN Web DocsのHTTPリクエストに関する情報](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)