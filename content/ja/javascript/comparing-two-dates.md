---
title:                "「二つの日付の比較」"
html_title:           "Javascript: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

JavaScriptでは、日付を比較することで、特定の日付と比較して、何かの結果を得ることができます。これは、期限や期間を管理する際に非常に便利です。

## 方法

まず第一に、二つの日付を取得します。これらは、Dateオブジェクトを使用して取得することができます。次に、比較演算子（<, >, <=, >=）を使用して、二つの日付を比較します。例えば、以下のコードは二つの日付を比較し、結果をコンソールに出力します。

```Javascript
let date1 = new Date("2021/01/01");
let date2 = new Date("2021/01/15");

if (date1 < date2) {
  console.log("date1はdate2よりも前です。");
}
```

実行結果は以下のようになります。

```
date1はdate2よりも前です。
```

## 深堀り

日付を比較する際には、注意しなければならない点がいくつかあります。まず、Dateオブジェクトを作成する際に、注意しなければならないのは、月を表すときに0が1月を表し、11が12月を表すという点です。また、日付が異なっていても、年や月・日が同じであれば、それでも等しいとみなされます。これらのポイントを抑えておくことで、正しく日付を比較することができます。

## はまわなくても

もし日付を比較する際に、他の言語でよく使われているような「日付を数値型に変換してから比較する」という手法を使いたい場合は、DateオブジェクトのgetTime()メソッドを使用することができます。このメソッドは、1970年1月1日からのミリ秒数を返すので、それを利用して日付を数値型に変換することができます。

## 参考リンク

- [MDN: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Dateオブジェクトの基礎](https://qiita.com/chihiro/items/7ce21bbe991ee87720f8)
- [Dateを比較するのが辛い人の為に。](https://qiita.com/yarron/items/60a15bba8bbaf5e27dd2#_%E6%AF%94%E8%BC%83%E3%83%AD%E3%82%B8%E3%83%83%E3%82%AF)