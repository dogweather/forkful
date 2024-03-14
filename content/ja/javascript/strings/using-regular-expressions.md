---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:29.708400-07:00
description: "JavaScript\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\
  \u3001\u6587\u5B57\u5217\u5185\u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\
  \u306B\u4E00\u81F4\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30D1\
  \u30BF\u30FC\u30F3\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3001\u62BD\u51FA\u3001\u304A\u3088\u3073\
  \u64CD\u4F5C\u306B\u4F7F\u7528\u3057\u3001\u7C21\u6F54\u306A\u30B3\u30FC\u30C9\u3067\
  \u5F37\u529B\u306A\u6587\u5B57\u5217\u51E6\u7406\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.661989-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\
  \u6587\u5B57\u5217\u5185\u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\u306B\
  \u4E00\u81F4\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30D1\u30BF\
  \u30FC\u30F3\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C6\
  \u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3001\u62BD\u51FA\u3001\u304A\u3088\u3073\u64CD\
  \u4F5C\u306B\u4F7F\u7528\u3057\u3001\u7C21\u6F54\u306A\u30B3\u30FC\u30C9\u3067\u5F37\
  \u529B\u306A\u6587\u5B57\u5217\u51E6\u7406\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\u3057\
  \u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ?

JavaScriptでの正規表現（regex）は、文字列内の文字の組み合わせに一致するために使用されるパターンです。プログラマーは、テキストの検索、抽出、および操作に使用し、簡潔なコードで強力な文字列処理操作を可能にします。

## 使い方:

### 基本的な一致

始めるにあたり、単純なregexパターンを作成し、文字列内で一致を見つけることができます。ここでは、"code"という単語を見つけます：

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### `String.prototype.match()`の使用

一致する配列を取得するには：

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### グローバル検索

すべての一致を見つけるには、`g`フラグを使用します：

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### 大文字小文字を区別しないマッチング

`i`フラグは大文字小文字を無視します：

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### テキストの置換

`String.prototype.replace()`を使用して、文字列の一部を置き換えます：

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### グループの使用

グループはパターンの一部をキャプチャできます：

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### サードパーティのライブラリ

JavaScriptの組み込みのregex機能は強力ですが、`XRegExp`のようなライブラリを使用すると、いくつかのタスクが簡素化されるかもしれません。これは、追加の構文やフラグを提供し、複雑なパターンをより読みやすくします：

```javascript
// XRegExpライブラリの例
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

このスニペットは、`XRegExp`を使用して文字列内のすべてのUnicode単語に一致する方法を示しており、JavaScriptの組み込み機能を超えた拡張文字セットを扱うライブラリの能力を紹介しています。
