---
title:                "HTMLの解析"
html_title:           "Javascript: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## 何で?
HTMLパースとは、HTMLコードを読み込んで、それをより分かりやすい形式に変換することです。プログラマーがHTMLをパースする理由は、HTMLコードを整形したり、データを抽出したりすることができるためです。

## 方法:
```Javascript
// HTMLコードをパースする方法
let html = "<html><body><h1>Hello World!</h1></body></html>";
let parser = new DOMParser();
let xmlDoc = parser.parseFromString(html, "text/html");

// タグ名を指定して要素を取得する例
let heading = xmlDoc.getElementsByTagName("h1");
console.log(heading[0].innerText);
// Output: "Hello World!"
```

```Javascript
// パースしたHTMLコードからデータを抽出する方法
let html = "<ul><li>Apple</li><li>Orange</li><li>Banana</li></ul>";
let parser = new DOMParser();
let xmlDoc = parser.parseFromString(html, "text/html");

// リストアイテムを配列として取得する例
let itemList = xmlDoc.getElementsByTagName("li");
for (let i = 0; i < itemList.length; i++) {
  console.log(itemList[i].innerText);
} 
// Output: "Apple", "Orange", "Banana"
```

## 詳細:
1. HTMLパースは古くから存在しており、ユーザーインターフェースの開発やスクレイピングなど、さまざまな用途で使用されてきました。
2. HTMLパースの代替手段として、正規表現やCSSセレクターなどがありますが、正規表現はコードが複雑になりやすく、CSSセレクターでは必要なデータを取得できないことがあります。
3. 実装上の詳細として、```DOMParser```クラスを使用することでHTMLコードをパースし、パースされた結果をXMLドキュメントとして取得できます。その後、XMLドキュメントから必要な要素を取得して操作することができます。

## さらに見る:
- [MDN Web Docs - DOMParser](https://developer.mozilla.org/ja/docs/Web/API/DOMParser)
- [w3schools - JavaScript DOMParser](https://www.w3schools.com/jsref/dom_obj_document.asp)
- [Codecademy - Introduction to the DOM](https://www.codecademy.com/learn/learn-the-dom)