---
title:                "現在の日付の取得"
date:                  2024-02-03T19:10:00.163649-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
JavaScriptで現在の日付を取得することは、基本的な作業であり、今日の日付と時刻の取得、および場合によっては操作を行います。プログラマーは、ウェブサイトやアプリケーションに日付を表示したり、ユーザーの操作を追跡したり、時間に敏感なデータを扱うためにこれを行います。

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
