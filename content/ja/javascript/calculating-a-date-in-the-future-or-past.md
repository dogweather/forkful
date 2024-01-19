---
title:                "未来または過去の日付の計算"
html_title:           "Javascript: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# JavaScriptで未来・過去の日付を計算する

## 1. 何それ？そしてなぜ？

未来・過去の日付計算は、指定した日付から特定の日数を加えたり減ずるりする処理です。これが必要な理由？プログラマーは期限、スケジュール、イベントなどの日時管理を実現するために使いますよ。

## 2. 使い方:

例を見てみましょう。

```Javascript
let now = new Date();

// 明日の日付を計算する
let tomorrow = new Date();
tomorrow.setDate(now.getDate() + 1);
console.log(tomorrow);

// 一週間前の日付を計算する
let lastWeek = new Date();
lastWeek.setDate(now.getDate() - 7);
console.log(lastWeek);
```

これらのコードは、明日と一週間前の日付を計算して表示します。

## 3. 深掘り:

過去・未来の日付計算は本当に古くからある技術です。絶対的な時間（Unixエポックタイムstampのような）から相対的な日付を計算する方法がよく使われています。

そして、計算の修正も必要なケースがありますね。例えば、閏年では、2月が28日ではなく29日です。JavaScriptのDateオブジェクトはこれを自動的に処理してくれます。

他にもライブラリを使う方法もあります。Moment.jsやDay.jsなどは使いやすいAPIを提供しています。でも、ネイティブのJavaScriptで要件が満たせるなら、それを選ぶ方がシンプルかもしれません。

## 4. 参考リンク:

- MDNのドキュメンテーション: [JavaScriptのDateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js: [公式ドキュメント](https://momentjs.com/)
- Day.js: [公式ドキュメント](https://day.js.org/)
  
使い方や歴史について詳しく知りたい方は、是非リンクをチェックしてみてくださいね。それでは、ハッピープログラミングを！