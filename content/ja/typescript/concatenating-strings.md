---
title:                "TypeScript: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結を行う理由は、文字列をより複雑な形に整形するためです。例えば、データベースから取得した情報を使ってメッセージを作成したり、ユーザーからの入力と固定のテキストを組み合わせたりすることができます。

## 方法

文字列の連結を行うには、`+`演算子を使用します。例えば、以下のようなコードを使って、2つの文字列を繋げることができます。

```TypeScript
let name = "山田"
let message = "こんにちは、" + name + "さん！";
console.log(message);
```

このコードの出力は、`こんにちは、山田さん！`となります。

また、`${}`を使って複数の値を繋げることもできます。例えば、以下のように書くことで、複数の変数や定数を簡単に組み合わせることができます。

```TypeScript
let firstName = "太郎";
let lastName = "田中";
let age = 30;
let message = `私の名前は${lastName}${firstName}です。年齢は${age}歳です。`;
console.log(message);
```

このコードの出力は、`私の名前は田中太郎です。年齢は30歳です。`となります。

## 深堀り

文字列の連結を行う時には、文字列として扱うことができることに注意しましょう。例えば、以下のように数字の10と文字列の"10"を結合すると、"1010"という文字列になります。

```TypeScript
let num1 = 10;
let num2 = "10";
let result = num1 + num2;
console.log(result);
```

このコードの出力は、`1010`となります。

また、`+=`演算子を使って文字列を追加することもできます。例えば、以下のように書くことで、前の値に新しい値を追加していきます。

```TypeScript
let message = "今日は";
message += "いい天気ですね。";
message += "外に出かけましょう！";
console.log(message);
```

このコードの出力は、`今日はいい天気ですね。外に出かけましょう！`となります。

## 参考

- [TypeScriptの公式ドキュメント - 文字列](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN web docs - 文字列の連結](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [Udemy - TypeScriptの基本](https://www.udemy.com/course/typescript-lesson-for-javascript-developers-template-literals/)