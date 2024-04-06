---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:29.708400-07:00
description: "\u4F7F\u3044\u65B9: \u59CB\u3081\u308B\u306B\u3042\u305F\u308A\u3001\
  \u5358\u7D14\u306Aregex\u30D1\u30BF\u30FC\u30F3\u3092\u4F5C\u6210\u3057\u3001\u6587\
  \u5B57\u5217\u5185\u3067\u4E00\u81F4\u3092\u898B\u3064\u3051\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\"code\"\u3068\u3044\
  \u3046\u5358\u8A9E\u3092\u898B\u3064\u3051\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.661989-06:00'
model: gpt-4-0125-preview
summary: "\u59CB\u3081\u308B\u306B\u3042\u305F\u308A\u3001\u5358\u7D14\u306Aregex\u30D1\
  \u30BF\u30FC\u30F3\u3092\u4F5C\u6210\u3057\u3001\u6587\u5B57\u5217\u5185\u3067\u4E00\
  \u81F4\u3092\u898B\u3064\u3051\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u3053\u3053\u3067\u306F\u3001\"code\"\u3068\u3044\u3046\u5358\u8A9E\u3092\u898B\
  \u3064\u3051\u307E\u3059\uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
