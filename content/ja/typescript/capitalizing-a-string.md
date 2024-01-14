---
title:    "TypeScript: 文字列を大文字に変換する"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜ

「文字列の先頭を大文字にする」というプログラミングの方法は、たくさんの人々が日常的に使用することがあります。例えば、表題を正式なものに変更したい場合や、データを整形する際にも使われます。この記事では、TypeScriptを使用して文字列を大文字にする方法を紹介します。

## 方法

文字列を大文字にするには、まず`toUpperCase()`メソッドを使用します。以下のように、`toUpperCase()`メソッドを文字列に直接適用することができます。

```TypeScript
let str = "hello world";
console.log(str.toUpperCase()); // "HELLO WORLD"
```

また、`charAt()`メソッドや`split()`メソッドを使用して、文字列を分割し、それぞれの単語の先頭文字を大文字に変換する方法もあります。具体的なコード例は以下のようになります。

```TypeScript
let str = "hello world";
let arr = str.split(" ");
for (let i = 0; i < arr.length; i++) {
  arr[i] = arr[i].charAt(0).toUpperCase() + arr[i].substring(1);
}
str = arr.join(" ");
console.log(str); // "Hello World"
```

## ディープダイブ

文字列を大文字にするメソッドには、`toLocaleUpperCase()`や`toUpperCase()`の他に`intl-standard`パッケージを使う方法もあります。これを使用することで、環境によって適切な文字列の大文字形式を生成することができます。

また、TypeScriptでは、ジェネリクスを使用して任意の型の文字列を大文字にすることもできます。コード例は以下のようになります。

```TypeScript
function capitalize<T>(str: T): T {
  // Converting str to stringを行う
  let s = "" + str;
  return s.charAt(0).toUpperCase() + s.substring(1);
}
console.log(capitalize("hello")); // "Hello"
console.log(capitalize(123)); //TypeError: s.charAt is not a function
```

## 参考リンク

- [String.prototype.toUpperCase() | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.charAt() | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.split() | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [intl-standard | npm](https://www.npmjs.com/package/intl-standard)