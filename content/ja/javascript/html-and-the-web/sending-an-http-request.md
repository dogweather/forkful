---
date: 2024-01-20 18:00:06.827493-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.672139-06:00'
model: gpt-4-1106-preview
summary: .
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## How to: (やり方)
```javascript
// XMLHttpRequestを使う古典的な方法
const xhr = new XMLHttpRequest();
xhr.open('GET', 'https://api.example.com/data', true);
xhr.onreadystatechange = function () {
  if (xhr.readyState === 4 && xhr.status === 200) {
    console.log(xhr.responseText);
  }
};
xhr.send();

// Fetch APIを使ったモダンな方法
fetch('https://api.example.com/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));

// async/awaitとFetch APIの組み合わせ
async function fetchData() {
  try {
    const response = await fetch('https://api.example.com/data');
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Error:', error);
  }
}

fetchData();
```

サンプル出力:
```javascript
// コンソールに取得したデータが表示される
{ "name": "Taro", "age": 30 }
```

## Deep Dive (詳細情報)
HTTPリクエストの送信はウェブの初期から行われている基本的行為です。最初はXMLHttpRequestが主流でしたが、プロミスによる構文の簡素化や、async/awaitの導入により、書きやすく読みやすいコードが可能になりました。Fetch APIは現在の標準で、ブラウザ内でHTTPリクエストを送るための推奨される方法です。XHRは古く、コードが複雑になりがちですが、古いブラウザをサポートするためにはまだ使われることがあります。Fetch APIはより簡潔でモダンな書き方ができ、プロミスベースなので非同期操作を扱いやすいです。

## See Also (関連情報)
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/ja/docs/Web/API/Fetch_API)
- [MDN Web Docs - XMLHttpRequest](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)
- [JavaScript.info - Fetch](https://javascript.info/fetch)
- [Google Developers - Introduction to Fetch](https://developers.google.com/web/updates/2015/03/introduction-to-fetch)
