---
title:                "文字列から日付をパースする"
aliases:
- /ja/javascript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:00.306501-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析することにより、プログラマはテキスト形式の日付表現をJavaScriptの`Date`オブジェクトに変換できます。これにより、日付の操作、比較、およびフォーマット操作を容易にします。このプロセスは、ユーザー入力を処理する際、データベースからデータを処理する際、または日付を文字列形式で通信するAPIと作業する際に不可欠です。

## 方法:
JavaScriptはネイティブに`Date.parse()`メソッドと`Date`コンストラクタを提供して日付文字列を解析します。しかし、これらのアプローチには、特に非標準の日付形式で異なるブラウザー間での制限と非整合性があります。これらの問題に対処するため、`Moment.js`や`date-fns`のようなサードパーティーのライブラリがその堅牢性と使いやすさで人気があります。

### ネイティブJavaScriptを使用して:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // 出力: Sun Apr 30 2023 14:55:00 GMT+0000 (協定世界時)
```

### Moment.jsを使用して:
まず、npm経由でMoment.jsをインストールするか、プロジェクトに含めます。次に：
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // 出力: Sun Apr 30 2023 14:55:00 GMT+0000
```

### date-fnsを使用して:
`date-fns`をプロジェクトに追加した後、以下のように日付文字列を解析します：
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // 出力: 2023-04-30T14:55:00.000Z
```

`Moment.js`と`date-fns`の両方は、さまざまな形式とロケールの扱いを含む、より包括的な解析機能を提供します。これにより、複雑なアプリケーションにとって好ましい選択となります。
