---
title:    "Javascript: 2つの日付の比較"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

JavaScriptで日付を比較する方法

## なぜ

日付を比較することは、プログラミングにおいて非常に重要なスキルです。日付を比較することで、特定の日付が過去のものか未来のものかを判断し、それに応じてプログラムを実行することができます。例えば、特定の日付が今日よりも前のものであれば、特定の処理を実行するように命令することができます。

## 使い方

日付を比較するには、一連のステップを踏む必要があります。

1. 日付を取得する
2. 日付を比較する
3. 比較結果を取得する

例えば、今日の日付を取得するには、次のようなコードを書きます。

```JavaScript
const today = new Date();
```

そして、比較したい日付を変数に代入し、比較演算子を使用して今日の日付と比較します。

```JavaScript
const comparisonDate = new Date("2020-01-01");
const result = comparisonDate < today;
```

このように、比較結果はtrueまたはfalseの値として取得することができます。

## 詳細を掘り下げる

日付を比較する際には、留意すべきいくつかのポイントがあります。

- 日付を表すオブジェクトを使用する際には、コンストラクタ関数の引数には月を表す数値ではなく、月のインデックスを表す数値を渡す必要があります。例えば、1月は0、12月は11となります。
- 比較する際には、日付でも時刻でもなく、ただの日付を比較するように注意しましょう。日付と時刻を比較すると、想定した結果とは異なる結果が得られる可能性があります。

## 参考リンク

- [MDN web docs - 日付を操作する](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TechAcademyマガジン - JavaScriptの日付を扱う方法](https://techacademy.jp/magazine/21464)
- [JavaScript.info - Dateオブジェクト](https://ja.javascript.info/date)

## 関連リンク

- [JavaScriptで時間を扱う方法](https://mycodinglab.com/javascript-time/)
- [JavaScript入門 - 日付の扱い方](https://javascript.programmer-reference.com/js-datetime/)