---
title:    "TypeScript: 「過去や未来の日付の計算」"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算する必要のある場面は様々あります。例えば、予定表にイベントを追加する際などです。今回はTypeScriptで日付を計算する方法を紹介します。

## 方法

まず、今日の日付を取得する必要があります。そのためにDateオブジェクトを使用し、現在の日付を取得します。

```TypeScript
const today: Date = new Date();
```

次に、今日の日付から未来や過去の日付が何日後や何日前かを計算して、目的の日付を取得します。具体的には、今回は15日後の日付を計算することにします。

```TypeScript
const futureDate: Date = new Date(today.getDate() + 15);
```

このコードを実行すると、15日後の日付が出力されます。例えば、今日が2021年10月20日の場合、futureDateには2021年11月4日が格納されます。

同様に、過去の日付を計算することもできます。例として、5日前の日付を計算する方法を紹介します。

```TypeScript
const pastDate: Date = new Date(today.getDate() - 5);
```

このコードを実行すると、5日前の日付が出力されます。例えば、今日が2021年10月20日の場合、pastDateには2021年10月15日が格納されます。

## ディープダイブ

Dateオブジェクトを使用することで、簡単に日付の計算が可能です。また、今回は日付を計算する際にgetDate()メソッドを使用しましたが、他にもgetDay()やgetMonth()など様々なメソッドが用意されています。詳細な情報を知りたい方は、公式ドキュメントを参考にしてみてください。

## 参考リンク

- [MDN web docs - JavaScript Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript公式ドキュメント - Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [TypeScriptで日付を操作する](https://qiita.com/piro0919/items/ed9b5ca11fdfa79db754)