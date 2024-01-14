---
title:    "TypeScript: 文字列の長さを見つける"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜ
*なぜ文字列の長さを見つけることに関わるのか？*
文字列の長さを見つけることは、テキスト処理において非常に重要です。例えば、文字列の長さを把握することで、入力されたテキストが指定された長さを超えていないかをチェックすることができます。また、文字列の長さはデータ構造やアルゴリズムの設計においても重要な役割を果たします。

## 方法
*どのように文字列の長さを見つけるのか？*
TypeScriptでは、`length`プロパティを使用することで簡単に文字列の長さを見つけることができます。

```TypeScript
let str: string = 'こんにちは！';
console.log(`入力文字列：${str}`);
console.log(`文字列の長さ：${str.length}`);

/*
出力結果：
入力文字列：こんにちは！
文字列の長さ：6
*/
```

他にも、文字列を配列として扱うこともできます。これにより、`for`ループや`forEach`メソッドを使用することで文字列の文字を1つずつ取得し、カウンターを使用して長さを計算することができます。

```TypeScript
let str: string = 'こんにちは！';
console.log(`入力文字列：${str}`);

// 配列として扱う
let strArray: string[] = str.split('');

// 長さを計算するカウンター
let length: number = 0;

// ループを使用してカウンターをインクリメント
for (let i: number = 0; i < strArray.length; i++) {
    length++;
}

console.log(`文字列の長さ：${length}`);

/*
出力結果：
入力文字列：こんにちは！
文字列の長さ：6
*/
```

## 詳細について
*文字列の長さを見つけることのさらなる情報*
文字列の長さを見つける際に注意することは、空白文字や句読点も長さに含まれるということです。また、日本語のような多バイト文字を含む文字列の場合、`length`プロパティでは正しい長さを取得できない可能性があります。この場合は、特殊な関数を使用することで正しい文字列の長さを取得することができます。

## 参考リンク
*参考になるリンク*
- [TypeScript Handbook - 文字列](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN Web Docs - String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Welmi Díaz - How to get length of a string in TypeScript](https://www.techiediary.com/typescript-string-length/)
- [Winfred Wani - How to get correct length of a string in TypeScript](https://blog.angular-university.io/typescript-2-type-system-how-to-convert-common-js-modules-to-typescript-npm-packages/)

## 参考文献
*参考になる文献*
- [TypeScript言語仕様 - 文字列](https://github.com/microsoft/TypeScript/blob/master/doc/spec.md#2.4.3)
- [ECMA International - ECMAScript 262 Language Specification](https://tc39.es/ecma262/#sec-string-objects)
- [企業IT系エンジニアブログ - JavaScriptの文字列処理の基本をマスターする](http://www.atmarkit.co.jp/ait/articles/1503/02/news039.html)
- [情報シークエンス演