---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:48.449883-07:00
description: "\u65B9\u6CD5: TypeScript\u306FJavaScript\u306E\u30B9\u30FC\u30D1\u30FC\
  \u30BB\u30C3\u30C8\u3067\u3042\u308B\u305F\u3081\u3001\u7D14\u7C8B\u306AJavaScript\u306E\
  \u30A2\u30D7\u30ED\u30FC\u30C1\u304B\u3089\u3001\u3088\u308A\u8907\u96D1\u307E\u305F\
  \u306F\u7279\u5B9A\u306E\u30E6\u30FC\u30B9\u30B1\u30FC\u30B9\u306E\u305F\u3081\u306B\
  \u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u5229\u7528\u3059\u308B\u65B9\u6CD5\u307E\u3067\u3001\u6587\u5B57\u5217\u3092\u5927\
  \u6587\u5B57\u5316\u3059\u308B\u3055\u307E\u3056\u307E\u306A\u65B9\u6CD5\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002 **\u7D14\u7C8B\u306AJavaScript\u30A2\u30D7\u30ED\u30FC\
  \u30C1:**."
lastmod: '2024-04-05T22:37:50.035084-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u306FJavaScript\u306E\u30B9\u30FC\u30D1\u30FC\u30BB\u30C3\u30C8\
  \u3067\u3042\u308B\u305F\u3081\u3001\u7D14\u7C8B\u306AJavaScript\u306E\u30A2\u30D7\
  \u30ED\u30FC\u30C1\u304B\u3089\u3001\u3088\u308A\u8907\u96D1\u307E\u305F\u306F\u7279\
  \u5B9A\u306E\u30E6\u30FC\u30B9\u30B1\u30FC\u30B9\u306E\u305F\u3081\u306B\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5229\u7528\
  \u3059\u308B\u65B9\u6CD5\u307E\u3067\u3001\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\
  \u5316\u3059\u308B\u3055\u307E\u3056\u307E\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法:
TypeScriptはJavaScriptのスーパーセットであるため、純粋なJavaScriptのアプローチから、より複雑または特定のユースケースのためにサードパーティのライブラリを利用する方法まで、文字列を大文字化するさまざまな方法を提供します。

**純粋なJavaScriptアプローチ:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// サンプル出力:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

この方法は直感的で、`charAt()`メソッドを使用して文字列の最初の文字にアクセスし、`toUpperCase()`でそれを大文字に変換します。その後、`slice(1)`メソッドで文字列の残りの部分を取得し、変更せずに残します。

**Lodashライブラリを使用する:**

[Lodash](https://lodash.com/)ライブラリをすでに使用しているプロジェクトでは、その`_.capitalize`関数を利用して、より少ないボイラープレートコードで同じ結果を得ることができます。

まず、Lodashをインストールします：

```bash
npm install lodash
```

次に、TypeScriptファイルで使用します：

```typescript
import * as _ from 'lodash';

// サンプル出力:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

注：Lodashの`_.capitalize`メソッドは、文字列の残りの部分を小文字に変換するため、常に望ましいわけではありません。

**正規表現を使用する:**

正規表現を使用すれば、文字列の最初の文字を大文字化する、特に文字列の各単語の最初の文字を大文字化する必要がある場合に、簡潔な方法を提供できます。

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// サンプル出力:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

この方法は、任意の単語境界に続く英数字文字(`\b\w`)を検索してそれぞれを大文字にする`replace()`関数を使用します。タイトルや見出しに特に便利です。
