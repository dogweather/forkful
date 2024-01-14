---
title:                "Javascript: 二つの日付を比較する"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜJavascriptで日付を比較するのか

日付を比較することは、プログラミングで非常に一般的な作業です。特に、2つの日付を比較することは、いくつかのタスクにおいて非常に役立つことがあります。例えば、ある日付と現在の日付を比較して、その日付が未来のものか過去のものかを判断することができます。また、ある日付の前後関係を知ることができます。Javascriptを使って日付を比較することにより、より複雑なタスクを簡単に実現することができます。

## 日付の比較の方法

```Javascript
// 日付を定義
let date1 = new Date("2021-07-15");
let date2 = new Date("2021-07-20");

// 日付の比較
if (date1 < date2) {
    console.log("date1はdate2よりも過去です");
} else if (date1 > date2) {
    console.log("date1はdate2よりも未来です");
} else {
    console.log("date1とdate2は同じ日付です");
}

// 日付の差を求める
let difference = date2 - date1;
console.log("date1からdate2までの日数 : " + difference / (1000 * 60 * 60 * 24));
```

上記のコードでは、まず2つの日付を定義し、その日付を比較しています。`<`や`>`を使って比較をすることで、2つの日付の前後関係を知ることができます。また、日付の差を求めることもできます。上記の例では、日数単位で差を求めることができますが、時間や分、秒単位でも差を求めることができます。

## 日付の比較の深堀り

日付の比較では、文字列や数値の比較とは少し異なる点があります。日付は、単に数値として比較することができません。そのため、`<`や`>`だけでなく、`===`や`!==`も使って比較する必要があります。

また、日付を直接文字列として比較することもできます。しかし、この方法では日付の書式によっては意図しない結果が返ってくる可能性があります。そのため、日付を比較する際には、`Date`オブジェクトを使って比較することをおすすめします。

# おわりに

日付を比較することは、プログラミングで非常に重要なスキルです。Javascriptを使って日付を比較する方法を学ぶことで、より複雑なタスクを簡単に実現することができるようになります。ぜひ、今後のプログラミングで日付を比較する際には、この記事を参考にしてみてください。

# 関連記事

- [日付を比較するためのDateオブジェクトの使い方](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [日付と時間の処理を容易にするMoment.jsの紹介](https://qiita.com/riversun/items/77d059823bf059589d2e)
- [Javascriptで日付を扱う際の注意点](https://tech.nikke