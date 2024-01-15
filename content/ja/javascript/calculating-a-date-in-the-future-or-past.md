---
title:                "未来または過去の日付を計算する"
html_title:           "Javascript: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
日付を未来や過去に計算する理由はいくつかあります。例えば、特定のイベントの日付を計算したり、期限を設定したりするためです。また、プログラムの中で日付を操作する必要がある場合にも役立ちます。

## How To
計算を行うためには、まずJavascriptのDateオブジェクトを使用して現在の日付を取得します。次に、この日付に対して「年」「月」「日」「時間」「分」「秒」などのメソッドを使用して、未来や過去の日付を計算します。

例えば、5日後の日付を計算する場合は以下のようになります。

```Javascript
let today = new Date();
let futureDate = new Date(today.getFullYear(), today.getMonth(), today.getDate() + 5);
console.log(futureDate);
```

上記のコードでは、現在の日付に`getDate()`メソッドを使用して5を加えています。そして、`new Date()`メソッドを使用して未来の日付を取得し、`console.log()`で出力しています。

過去の日付を計算する場合も同様に、`getDate()`メソッドを使用して減算すれば良いです。

また、特定の日付を基準にして計算する方法もあります。例えば、2021年1月1日から5日後の日付を計算する場合は以下のようになります。

```Javascript
let baseDate = new Date("2021/01/01");
let futureDate = new Date(baseDate.getFullYear(), baseDate.getMonth(), baseDate.getDate() + 5);
console.log(futureDate);
```

このように、基準となる日付を指定して計算をすることで、より柔軟性のある日付の計算が可能になります。

## Deep Dive
日付の計算は、プログラミングでよく使われる機能ですが、計算に伴う注意点もあります。例えば、日付の計算では時差や夏時間などの影響を受ける可能性があります。そのため、国際的なアプリケーションを開発する場合はこれらの問題に対応する必要があります。

また、日付の計算の際には、タイムゾーンを考慮する必要もあります。JavascriptのDateオブジェクトは、コンピューターのローカルタイムゾーンを使用してデータを生成するため、異なるタイムゾーンを使用する場合は注意が必要です。

## See Also
- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Web Docs: Date.prototype.setDate()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate)
- [Qiita: Javascriptで日付と時刻を操作する方法](https://qiita.com/coke_kitkat/items/7bf6bf81ae7f585e350c)