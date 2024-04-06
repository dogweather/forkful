---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:44.228994-07:00
description: "\u65B9\u6CD5\uFF1A JavaScript\u306B\u306F\u3001\u6587\u5B57\u5217\u3092\
  \u76F4\u63A5\u5927\u6587\u5B57\u5316\u3059\u308B\u7D44\u307F\u8FBC\u307F\u306E\u65B9\
  \u6CD5\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u57FA\u672C\u7684\u306A\u6587\
  \u5B57\u5217\u64CD\u4F5C\u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u3066\u7C21\u5358\u306B\
  \u5B9F\u88C5\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:42.143194-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A JavaScript\u306B\u306F\u3001\u6587\u5B57\u5217\u3092\u76F4\
  \u63A5\u5927\u6587\u5B57\u5316\u3059\u308B\u7D44\u307F\u8FBC\u307F\u306E\u65B9\u6CD5\
  \u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u57FA\u672C\u7684\u306A\u6587\u5B57\
  \u5217\u64CD\u4F5C\u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u3066\u7C21\u5358\u306B\u5B9F\
  \u88C5\u3067\u304D\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
JavaScriptには、文字列を直接大文字化する組み込みの方法はありませんが、基本的な文字列操作方法を使用して簡単に実装できます。

### 標準的なJavaScriptを使用
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // 出力: "Hello world"
```

### ES6バージョン
ES6のテンプレートリテラルを使用すると、より簡潔な方法で関数を書くことができます：
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // 出力: "Hello ES6"
```

### Lodashを使用
Lodashは、JavaScriptの値、文字列を含む、を操作・作業するための広範な関数を提供する人気のサードパーティのユーティリティライブラリです。Lodashを使用して文字列を大文字化するには：
```javascript
// まだの方は、まずlodashをインストールしてください: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // 出力: "Lodash example"
```
_Lodashは最初の文字を大文字にするだけでなく、文字列の残りを小文字に変換することに注意してください。これはプレーンなJavaScriptの実装とは少し異なります。_

### CSSを使用（表示目的のみ）
目的がUIでテキストを大文字化することであれば、CSSを使用できます：
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- "Hello css"として表示されます -->
```
**注:** この方法は、JavaScript自体の文字列を変更するのではなく、ウェブページ上でテキストがどのように見えるかを変更します。
