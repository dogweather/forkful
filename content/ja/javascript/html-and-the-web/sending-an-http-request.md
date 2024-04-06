---
date: 2024-01-20 18:00:06.827493-07:00
description: "How to: (\u3084\u308A\u65B9) HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\
  \u9001\u4FE1\u306F\u30A6\u30A7\u30D6\u306E\u521D\u671F\u304B\u3089\u884C\u308F\u308C\
  \u3066\u3044\u308B\u57FA\u672C\u7684\u884C\u70BA\u3067\u3059\u3002\u6700\u521D\u306F\
  XMLHttpRequest\u304C\u4E3B\u6D41\u3067\u3057\u305F\u304C\u3001\u30D7\u30ED\u30DF\
  \u30B9\u306B\u3088\u308B\u69CB\u6587\u306E\u7C21\u7D20\u5316\u3084\u3001async/await\u306E\
  \u5C0E\u5165\u306B\u3088\u308A\u3001\u66F8\u304D\u3084\u3059\u304F\u8AAD\u307F\u3084\
  \u3059\u3044\u30B3\u30FC\u30C9\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3057\u305F\
  \u3002Fetch\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.540463-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1\
  \u306F\u30A6\u30A7\u30D6\u306E\u521D\u671F\u304B\u3089\u884C\u308F\u308C\u3066\u3044\
  \u308B\u57FA\u672C\u7684\u884C\u70BA\u3067\u3059\u3002\u6700\u521D\u306FXMLHttpRequest\u304C\
  \u4E3B\u6D41\u3067\u3057\u305F\u304C\u3001\u30D7\u30ED\u30DF\u30B9\u306B\u3088\u308B\
  \u69CB\u6587\u306E\u7C21\u7D20\u5316\u3084\u3001async/await\u306E\u5C0E\u5165\u306B\
  \u3088\u308A\u3001\u66F8\u304D\u3084\u3059\u304F\u8AAD\u307F\u3084\u3059\u3044\u30B3\
  \u30FC\u30C9\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3057\u305F\u3002Fetch API\u306F\
  \u73FE\u5728\u306E\u6A19\u6E96\u3067\u3001\u30D6\u30E9\u30A6\u30B6\u5185\u3067HTTP\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u305F\u3081\u306E\u63A8\u5968\u3055\u308C\
  \u308B\u65B9\u6CD5\u3067\u3059\u3002XHR\u306F\u53E4\u304F\u3001\u30B3\u30FC\u30C9\
  \u304C\u8907\u96D1\u306B\u306A\u308A\u304C\u3061\u3067\u3059\u304C\u3001\u53E4\u3044\
  \u30D6\u30E9\u30A6\u30B6\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\u305F\u3081\u306B\
  \u306F\u307E\u3060\u4F7F\u308F\u308C\u308B\u3053\u3068\u304C\u3042\u308A\u307E\u3059\
  \u3002Fetch API\u306F\u3088\u308A\u7C21\u6F54\u3067\u30E2\u30C0\u30F3\u306A\u66F8\
  \u304D\u65B9\u304C\u3067\u304D\u3001\u30D7\u30ED\u30DF\u30B9\u30D9\u30FC\u30B9\u306A\
  \u306E\u3067\u975E\u540C\u671F\u64CD\u4F5C\u3092\u6271\u3044\u3084\u3059\u3044\u3067\
  \u3059\u3002"
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
