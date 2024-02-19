---
aliases:
- /ja/javascript/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:44.228994-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\
  \u306B\u5909\u63DB\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u306F\u305D\u306E\u307E\
  \u307E\u306B\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\
  \u306E\u64CD\u4F5C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u8A2D\u5B9A\u3001\u540D\u524D\u3084\u30BF\u30A4\u30C8\u30EB\
  \u306E\u8868\u793A\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\
  \u30A4\u30B9\u30C6\u30AD\u30B9\u30C8\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u8A3C\u3059\
  \u308B\u305F\u3081\u306B\u3001JavaScript\u3067\u4E00\u822C\u7684\u306B\u884C\u308F\
  \u308C\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.253414
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\
  \u306B\u5909\u63DB\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u306F\u305D\u306E\u307E\
  \u307E\u306B\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\
  \u306E\u64CD\u4F5C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u8A2D\u5B9A\u3001\u540D\u524D\u3084\u30BF\u30A4\u30C8\u30EB\
  \u306E\u8868\u793A\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\
  \u30A4\u30B9\u30C6\u30AD\u30B9\u30C8\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u8A3C\u3059\
  \u308B\u305F\u3081\u306B\u3001JavaScript\u3067\u4E00\u822C\u7684\u306B\u884C\u308F\
  \u308C\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく理由？
文字列を大文字化するとは、文字列の最初の文字を大文字に変換し、残りの文字はそのままにすることを意味します。この操作は、ユーザー入力のフォーマット設定、名前やタイトルの表示、ユーザーインターフェイステキストの一貫性を保証するために、JavaScriptで一般的に行われます。

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
