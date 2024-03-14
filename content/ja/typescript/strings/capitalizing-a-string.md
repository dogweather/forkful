---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:48.449883-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\
  \u5B57\u3092\u5C0F\u6587\u5B57\u304B\u3089\u5927\u6587\u5B57\u306B\u5909\u66F4\u3059\
  \u308B\u64CD\u4F5C\u3067\u3001\u901A\u5E38\u3001\u6587\u5B57\u5217\u306E\u6B8B\u308A\
  \u306E\u90E8\u5206\u306F\u5909\u66F4\u3055\u308C\u307E\u305B\u3093\u3002\u3053\u306E\
  \u64CD\u4F5C\u306F\u4E00\u822C\u7684\u306B\u3001\u56FA\u6709\u540D\u8A5E\u3084\u6587\
  \u306E\u59CB\u307E\u308A\u304C\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306B\u304A\u3044\
  \u3066\u6587\u6CD5\u898F\u5247\u306B\u5F93\u3046\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u306B\u4F7F\u7528\u3055\u308C\u3001\u51FA\u529B\u3092\u30D7\u30ED\u30D5\u30A7\
  \u30C3\u30B7\u30E7\u30CA\u30EB\u3067\u8AAD\u307F\u3084\u3059\u3044\u3082\u306E\u306B\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.734714-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\
  \u5B57\u3092\u5C0F\u6587\u5B57\u304B\u3089\u5927\u6587\u5B57\u306B\u5909\u66F4\u3059\
  \u308B\u64CD\u4F5C\u3067\u3001\u901A\u5E38\u3001\u6587\u5B57\u5217\u306E\u6B8B\u308A\
  \u306E\u90E8\u5206\u306F\u5909\u66F4\u3055\u308C\u307E\u305B\u3093\u3002\u3053\u306E\
  \u64CD\u4F5C\u306F\u4E00\u822C\u7684\u306B\u3001\u56FA\u6709\u540D\u8A5E\u3084\u6587\
  \u306E\u59CB\u307E\u308A\u304C\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306B\u304A\u3044\
  \u3066\u6587\u6CD5\u898F\u5247\u306B\u5F93\u3046\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u306B\u4F7F\u7528\u3055\u308C\u3001\u51FA\u529B\u3092\u30D7\u30ED\u30D5\u30A7\
  \u30C3\u30B7\u30E7\u30CA\u30EB\u3067\u8AAD\u307F\u3084\u3059\u3044\u3082\u306E\u306B\
  \u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
文字列を大文字化するとは、与えられた文字列の最初の文字を小文字から大文字に変更する操作で、通常、文字列の残りの部分は変更されません。この操作は一般的に、固有名詞や文の始まりがテキスト処理において文法規則に従うようにするために使用され、出力をプロフェッショナルで読みやすいものにします。

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
