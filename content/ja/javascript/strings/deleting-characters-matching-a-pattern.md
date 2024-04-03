---
date: 2024-01-20 17:42:45.860532-07:00
description: "How to: (\u65B9\u6CD5) JavaScript \u3067\u30D1\u30BF\u30FC\u30F3\u306B\
  \u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u57FA\u672C\
  \u7684\u306A\u65B9\u6CD5\u306F `replace()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3046\
  \u3053\u3068\u3067\u3059\u3002\u6B63\u898F\u8868\u73FE\u3092\u7528\u3044\u3066\u30DE\
  \u30C3\u30C1\u3055\u305B\u305F\u3044\u6587\u5B57\u3092\u6307\u5B9A\u3057\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.653844-06:00'
model: gpt-4-1106-preview
summary: "JavaScript \u3067\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\u30C3\u30C1\u3059\u308B\
  \u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u57FA\u672C\u7684\u306A\u65B9\u6CD5\u306F\
  \ `replace()` \u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\
  \u3002\u6B63\u898F\u8868\u73FE\u3092\u7528\u3044\u3066\u30DE\u30C3\u30C1\u3055\u305B\
  \u305F\u3044\u6587\u5B57\u3092\u6307\u5B9A\u3057\u307E\u3059."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
JavaScript でパターンにマッチする文字を削除する基本的な方法は `replace()` メソッドを使うことです。正規表現を用いてマッチさせたい文字を指定します。

```javascript
let originalString = "こんにちは、1234世界！";
let newString = originalString.replace(/[0-9]/g, '');
console.log(newString); // "こんにちは、世界！"
```

上記のコードでは、`replace()` メソッドを使って数字を削除しています。`/[0-9]/g` は「0から9までの数字をグローバルに検索する」という意味の正規表現です。

## Deep Dive (深掘り)
JavaScriptで文字を削除する機能は古くからあり、`replace()` メソッドは ECMAScript の初期バージョンから使用されています。しかし、ECMAScript 2015 (ES6) 以降、テンプレートリテラルやアロー関数などの新機能により、コードの書き方がより直感的になりました。

別の方法として、`split()` と `join()` メソッドを使うテクニックもあります。

```javascript
let originalString = "さようなら、5678世界！";
let newString = originalString.split('').filter(char => !/[0-9]/.test(char)).join('');
console.log(newString); // "さようなら、世界！"
```

このコードでは、文字列を一文字ずつ分解して数字以外の文字をフィルタリングし、再び結合しています。しかし `replace()` の方が簡潔で高速です。

## See Also (関連情報)
- [MDN Web Docs: replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [MDN Web Docs: split()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN Web Docs: join()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Array/join)
- [ECMAScript 2015 (ES6) Features](https://github.com/lukehoban/es6features)

これらのリンク先では、`replace()` メソッドや正規表現、そして他の文字列操作メソッドについてさらに詳しく学ぶことができます。
