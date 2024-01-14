---
title:    "Javascript: 「2つの日付の比較」"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

日々のコーディングで、時々JavaScriptで日付を比較する必要があります。コードの正確さを保つために、日付を正しく比較することが非常に重要です。

## 方法

日付を比較するために、JavaScriptにはいくつかの便利なメソッドがあります。例えば、`Date()`コンストラクターを使用して新しい日付オブジェクトを作成することができます。次に、`getTime()`メソッドを使用して、日付をミリ秒単位の数値に変換します。そして、この数値を比較することで日付の大小を判断できます。以下は、実際にコードを使用した例です。

```Javascript
let date1 = new Date('2020/01/01');
let date2 = new Date('2020/05/01');

console.log(date1.getTime() < date2.getTime()); // 出力結果: true
```
また、`getFullYear()`、`getMonth()`、`getDate()`メソッドを使用して、日付の年、月、日を取得し、それぞれ比較することもできます。

```Javascript
let date1 = new Date('2020/01/01');
let date2 = new Date('2020/01/03');

console.log(date1.getFullYear() === date2.getFullYear()); // 出力結果: true
console.log(date1.getMonth() === date2.getMonth()); // 出力結果: true
console.log(date1.getDate() < date2.getDate()); // 出力結果: true
```

## 深堀り

日付を比較する場合、注意しなければならない点が1つあります。それは、JavaScriptにおける日付オブジェクトは、時差の影響を受けるということです。つまり、例えば`2020/01/01`という日付を日本時間で比較する場合、実際には時差により1日前の`2019/12/31`として扱われます。このような場合は、可能な限りUTCを使用することをおすすめします。

## See Also

- [JavaScriptで日付を比較する方法](https://techacademy.jp/magazine/18006)
- [JavaScriptで時刻を考慮しない日付の比較をする](https://qiita.com/nek_0k_/items/8ffa7d3fa957324757a8)
- [日付を比較する際の注意点](https://qiita.com/muran001/items/b623c1a47f065ab3e3f3)