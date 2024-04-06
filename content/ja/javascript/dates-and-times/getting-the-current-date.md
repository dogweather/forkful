---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:00.163649-07:00
description: "\u65B9\u6CD5\uFF1A \u30D0\u30CB\u30E9JavaScript\u3067\u306F\u3001`Date`\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u7528\u3057\u3066\u65E5\u4ED8\u3068\u6642\
  \u523B\u3092\u64CD\u4F5C\u3057\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u73FE\
  \u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u3092\u53D6\u5F97\u3059\u308B\u65B9\u6CD5\
  \u3092\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.477771-06:00'
model: gpt-4-0125-preview
summary: ''
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
