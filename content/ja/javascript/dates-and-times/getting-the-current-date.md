---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:00.163649-07:00
description: "JavaScript\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\
  \u308B\u3053\u3068\u306F\u3001\u57FA\u672C\u7684\u306A\u4F5C\u696D\u3067\u3042\u308A\
  \u3001\u4ECA\u65E5\u306E\u65E5\u4ED8\u3068\u6642\u523B\u306E\u53D6\u5F97\u3001\u304A\
  \u3088\u3073\u5834\u5408\u306B\u3088\u3063\u3066\u306F\u64CD\u4F5C\u3092\u884C\u3044\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\
  \u30B5\u30A4\u30C8\u3084\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u65E5\
  \u4ED8\u3092\u8868\u793A\u3057\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u306E\u64CD\
  \u4F5C\u3092\u8FFD\u8DE1\u3057\u305F\u308A\u3001\u6642\u9593\u306B\u654F\u611F\u306A\
  \u30C7\u30FC\u30BF\u3092\u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.691625-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u57FA\u672C\u7684\u306A\u4F5C\u696D\u3067\u3042\u308A\u3001\
  \u4ECA\u65E5\u306E\u65E5\u4ED8\u3068\u6642\u523B\u306E\u53D6\u5F97\u3001\u304A\u3088\
  \u3073\u5834\u5408\u306B\u3088\u3063\u3066\u306F\u64CD\u4F5C\u3092\u884C\u3044\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30B5\
  \u30A4\u30C8\u3084\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u65E5\u4ED8\
  \u3092\u8868\u793A\u3057\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u306E\u64CD\u4F5C\
  \u3092\u8FFD\u8DE1\u3057\u305F\u308A\u3001\u6642\u9593\u306B\u654F\u611F\u306A\u30C7\
  \u30FC\u30BF\u3092\u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
バニラJavaScriptでは、`Date`オブジェクトを使用して日付と時刻を操作します。ここでは、現在の日付と時刻を取得する方法を示します：

```javascript
const currentDate = new Date();
console.log(currentDate); // 例：Fri Apr 14 2023 12:34:56 GMT+0100 (British Summer Time)
```

よりユーザーフレンドリーな形式で日付のみを表示するには、`toLocaleDateString()`のようなメソッドを使用できます：

```javascript
console.log(currentDate.toLocaleDateString()); // 例：4/14/2023
```

形式をより細かく制御するには、*Moment.js*や*date-fns*のようなサードパーティライブラリが非常に人気がありますが、Moment.jsは現在メンテナンスモードのレガシープロジェクトと見なされていることに注意が必要です。

*Moment.js*を使用する：

```javascript
const moment = require('moment'); // Node.jsを仮定するか、モジュールバンドラを使用
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // 例：2023-04-14
```

必要なものだけをインポートできるようにモジュール化を強調する*date-fns*を使用する場合：

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // 例：2023-04-14
```

それぞれのアプローチは、ビルトインの`Date`オブジェクトからライブラリを通じて利用可能な、より洗練されたフォーマッティングや操作の能力に至るまで、JavaScriptで日付を扱うための異なる利便性と柔軟性のレベルを提供します。
