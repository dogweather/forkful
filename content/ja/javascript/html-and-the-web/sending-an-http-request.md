---
date: 2024-01-20 18:00:06.827493-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\
  \u3046\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\
  \u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u9001\u3063\u305F\u308A\u3059\u308B\
  \u884C\u70BA\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u60C5\
  \u5831\u306E\u3084\u308A\u53D6\u308A\u3092\u3057\u3066\u30A2\u30D7\u30EA\u3084\u30A6\
  \u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u6A5F\u80FD\u3092\u5B9F\u73FE\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.672139-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\u3046\
  \u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\u3057\
  \u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u9001\u3063\u305F\u308A\u3059\u308B\u884C\
  \u70BA\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u60C5\u5831\
  \u306E\u3084\u308A\u53D6\u308A\u3092\u3057\u3066\u30A2\u30D7\u30EA\u3084\u30A6\u30A7\
  \u30D6\u30DA\u30FC\u30B8\u306E\u6A5F\u80FD\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
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
