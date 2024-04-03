---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:56.070508-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.773130-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u306E\u30C6\u30AD\u30B9\u30C8\u8868\
  \u73FE\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u64CD\u4F5C\u30FB\u5206\u6790\u3067\
  \u304D\u308B\u5F62\u5F0F\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\
  \u306E\u53D6\u308A\u6271\u3044\u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\u4ED8\
  \u304D\u30C7\u30FC\u30BF\u306E\u8A18\u9332\u3001API\u3068\u306E\u63A5\u7D9A\u3092\
  \u53EF\u80FD\u306B\u3057\u3001\u3088\u308A\u6A5F\u80FD\u7684\u3067\u30E6\u30FC\u30B6\
  \u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u4E00\u822C\u7684\u306A\u4F5C\u696D\u3067\
  \u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法:
TypeScriptはJavaScriptのスーパーセットであり、文字列から日付を解析する場合、Dateオブジェクトに依存しています。しかし、JS/TSで日付を扱うとき、Dateオブジェクトの特性により、コードが冗長になったり、不正確になったりすることがあります。ここでは、基本的な例と、より堅牢なソリューションのために人気のあるライブラリ`date-fns`を使用するアプローチを紹介します。

### JavaScriptのDateオブジェクトを使用
```typescript
// Dateコンストラクタを使用した基本的な解析
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// GMTの出力: "Fri Apr 21 2023 15:00:00 GMT+0000 (協定世界時)"
```

この方法はISO形式の文字列やその他いくつかの日付形式で機能しますが、ブラウザーやロケールによっては不明瞭な形式で結果が一貫しないことがあります。

### date-fnsの使用
`date-fns`ライブラリは、直感的で一貫性のある日付の取り扱いを提供します。これはモジュラー型のライブラリであり、必要な部分だけを含めることができるため、バンドルサイズを削減します。

まず`date-fns`をインストールします:

```sh
npm install date-fns
```

その後、文字列から日付を解析するために使用します:

```typescript
import { parseISO, format } from 'date-fns';

// ISO文字列の解析
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// 日付のフォーマット（例えば、人が読める形式に）
console.log(format(parsedDate, "PPPpp")); 
// 出力: "2023年4月21日午後3時00分"（出力はロケールによって異なる場合があります）
```

`date-fns`はさまざまな形式とロケールをサポートしているため、異なるユーザーリージョンにまたがる正確な日付の解析とフォーマットが必要なアプリケーションにとって堅固な選択となります。
