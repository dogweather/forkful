---
title:    "Javascript: 「未来や過去の日付を計算する」"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

未来や過去の日付を計算することが重要なのか、わずか1〜2文で説明します。

## 方法

日付を未来や過去に移動するためのコードの例と、 ```Javascript ... ```のコードブロック内のサンプル出力を提供します。

例えば、『2021年5月25日から2日後の日付を計算する』には、以下のようなコードが使用されます。

```Javascript 
const today = new Date();
const futureDate = new Date(today.getFullYear(), today.getMonth(), today.getDate() + 2);
console.log(futureDate);
```

出力：

``` Javascript
Wed Jun 09 2021 00:00:00 GMT+0900 (日本標準時)
```

## ディープダイブ

日付の計算は、時差や夏時間など、さまざまな要因によって影響を受けることがあります。未来や過去の日付を正確に計算するためには、それらの要因を考慮に入れる必要があります。また、JavascriptのDateオブジェクトには便利なメソッドがいくつかあり、それらを活用することで簡単に日付を計算することができます。

## 詳しくは

「JavascriptのDateオブジェクトを使用した日付の計算方法」: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date

「夏時間を考慮した日付の計算方法」: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat

## 関連記事

## もっと詳しく知りたい方は、以下の記事も参考にしてみてください。

「Javascriptの日付処理について知る」: https://qiita.com/k-ikegami/items/786b48f20bcd09c86441