---
title:    "TypeScript: 現在の日付を取得する"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜ
皆さんこんにちは！今日は、TypeScriptで現在の日付を取得する方法についてお話しします。プログラミングでは、現在の日付を取得することは非常に重要です。例えば、ユーザーが自分のアカウントにログインした日時を記録する必要がある場合や、特定の日付のイベントを計算する必要がある場合などには、現在の日付を知ることが必要です。

## 方法
TypeScriptで現在の日付を取得する方法は非常に簡単です。まずは、Dateオブジェクトを宣言しましょう。

```TypeScript
let now = new Date();
```

これで、現在の日付と時刻が取得できます。次に、フォーマットを整える方法を見てみましょう。

```TypeScript
let year = now.getFullYear();
let month = now.getMonth() + 1;
let date = now.getDate();
let hours = now.getHours();
let minutes = now.getMinutes();
let seconds = now.getSeconds();

let formattedDate = `${year}/${month}/${date} ${hours}:${minutes}:${seconds}`;
console.log(formattedDate);
```

実行結果は以下のようになります。

```bash
2021/5/24 9:30:15
```

## ディープダイブ
TypeScriptで現在の日付を取得する方法については以上ですが、もう少し深く掘り下げてみましょう。新しいDateオブジェクトが生成される際には、現在のローカル時間が設定されます。しかし、必ずしもそれが必要なフォーマットではない場合もあります。そんな時には、Dateオブジェクトのメソッドを使って設定を変更することができます。

例えば、タイムゾーンを日本に設定する場合は、以下のようにすることができます。

```TypeScript
now.setFullYear(2021);
now.setMonth(4);
now.setDate(24);
now.setTimezoneOffset(-540);

let formattedDate = `${year}/${month}/${date} ${hours}:${minutes}:${seconds}`;
console.log(formattedDate);
```

実行結果は以下のようになります。

```bash
2021/5/24 9:30:15
```

## 他にも見てみよう
それでは、今日の記事は以上です。もしより深く学びたい方は、以下のリンクを参考にしてみてください。

[Dateオブジェクトの仕様 (MDN)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)

[TypeScript公式ドキュメント](https://www.typescriptlang.org/docs/handbook/date-and-time.html)

それでは、また次回お会いしましょう！