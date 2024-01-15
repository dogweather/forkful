---
title:                "未来または過去の日付を計算する"
html_title:           "TypeScript: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

未来や過去の日付を計算する理由は多岐に渡ります。例えば、締め切りやイベントの日程を把握したり、将来の計画を立てたりする際に便利です。

## How To

日付の計算を行うには、TypeScriptの組み込み関数である`Date()`を使用します。この関数は、現在の日付を取得することができます。そして、JavaScriptのDateオブジェクトを使って未来や過去の日付を計算することができます。例えば、現在の日付に10日を加算するコードは以下のようになります。

```TypeScript
let currentDate = new Date(); //現在の日付を取得
currentDate.setDate(currentDate.getDate() + 10); //10日追加
console.log(currentDate); //コンソールに出力される日付は入力した日付の10日後になる
```

また、日付を加算するだけでなく、減算することもできます。例えば、現在の日付から1年前の日付を求めるコードは以下のようになります。

```TypeScript
let currentDate = new Date();
currentDate.setFullYear(currentDate.getFullYear() - 1); //1年減算
console.log(currentDate);
```

これらのコードを実行すると、現在の日付に加算または減算された日付がコンソールに表示されます。

## Deep Dive

TypeScriptのDateオブジェクトには、日付を加算したり減算したりするための多くのメソッドがあります。これらのメソッドを使いこなすことで、日付の計算をより柔軟に行うことができます。また、時差を考慮した日付の計算や、特定の曜日を指定して日付を求めることも可能です。

しかしながら、日付の計算には多くの注意点があります。例えば、閏年やオーバーフローなどが考慮されないと、意図しない結果になる可能性があります。そのため、日付を計算する際は、組み込み関数を利用するだけでなく、ライブラリを使用することをおすすめします。

## See Also

* [TypeScript Dateオブジェクトの公式ドキュメント](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
* [JavaScriptのDateオブジェクトの公式ドキュメント (英語)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)