---
title:    "TypeScript: 未来または過去の日付を計算する"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算することの重要性について説明します。私たちは日々、日付や時間を意識して生活していますが、時には未来や過去の日付を知ることが必要になる場合があります。例えば、誕生日や予定したイベントの日付を確認するために未来の日付を計算したい場合や、特定の日付を基準に過去の日付を計算したい場合があります。そのようなときには、日付を計算することがとても便利です。

## 方法

まず、未来の日付を計算する方法を説明します。TypeScriptでは、日付を計算するためにDateオブジェクトを使用します。以下のコードを参考にしてください。

```TypeScript
let currentDate = new Date(); //現在の日付を取得
let futureDate = new Date(); //計算したい未来の日付を保持する変数
futureDate.setDate(currentDate.getDate() + 7); //未来の日付を7日後に設定
console.log(futureDate); //現在の日付から7日後の日付が出力される
```

次に、過去の日付を計算する方法を説明します。同じくDateオブジェクトを使用しますが、計算する日付を過去のものに設定する必要があります。以下のコードが参考になります。

```TypeScript
let currentDate = new Date(); //現在の日付を取得
let pastDate = new Date(); //計算したい過去の日付を保持する変数
pastDate.setDate(currentDate.getDate() - 10); //過去の日付を10日前に設定
console.log(pastDate); //現在の日付から10日前の日付が出力される
```

## 深堀り

未来や過去の日付を計算する際には、Dateオブジェクトのメソッドを活用することが重要です。例えば、setDateメソッドの引数に任意の数字を渡すことで、指定した日数分の日付を増減させることができます。また、getFullYearやgetMonthといったメソッドを使うことで、年や月を取得することも可能です。日付の計算は日々のプログラミングで頻繁に使用するため、これらのメソッドを覚えておくと便利です。

## 参考リンク

- [TypeScriptの公式ドキュメント](https://www.typescriptlang.org/docs)
- [MDN Web Docs: Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [プログラミング初心者でも分かる、日付操作の基本](https://techacademy.jp/magazine/11403)