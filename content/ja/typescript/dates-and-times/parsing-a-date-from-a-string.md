---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:56.070508-07:00
description: "\u65B9\u6CD5:\u2026"
lastmod: '2024-03-13T22:44:41.773130-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u306FJavaScript\u306E\u30B9\u30FC\u30D1\u30FC\u30BB\u30C3\u30C8\
  \u3067\u3042\u308A\u3001\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\
  \u3059\u308B\u5834\u5408\u3001Date\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u4F9D\
  \u5B58\u3057\u3066\u3044\u307E\u3059\u3002\u3057\u304B\u3057\u3001JS/TS\u3067\u65E5\
  \u4ED8\u3092\u6271\u3046\u3068\u304D\u3001Date\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306E\u7279\u6027\u306B\u3088\u308A\u3001\u30B3\u30FC\u30C9\u304C\u5197\u9577\u306B\
  \u306A\u3063\u305F\u308A\u3001\u4E0D\u6B63\u78BA\u306B\u306A\u3063\u305F\u308A\u3059\
  \u308B\u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\
  \u57FA\u672C\u7684\u306A\u4F8B\u3068\u3001\u3088\u308A\u5805\u7262\u306A\u30BD\u30EA\
  \u30E5\u30FC\u30B7\u30E7\u30F3\u306E\u305F\u3081\u306B\u4EBA\u6C17\u306E\u3042\u308B\
  \u30E9\u30A4\u30D6\u30E9\u30EA`date-fns`\u3092\u4F7F\u7528\u3059\u308B\u30A2\u30D7\
  \u30ED\u30FC\u30C1\u3092\u7D39\u4ECB\u3057\u307E\u3059."
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
