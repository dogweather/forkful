---
title:    "TypeScript: 文字列の連結"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結を行うことの妥当性を説明するための 1~2 文について。

コンピュータープログラムにおいて、文字列はよく利用されるデータ型です。文字列を連結することによって、複数の文字列を一つの大きな文字列として結合することができます。これにより、より複雑な処理を行うことが可能になります。

## 連結の仕方

連結する際に使用する TypeScript のコード例と、それぞれの出力結果を下記のコードブロックで示します。

```TypeScript
// 文字列の宣言
let firstName: string = "太郎";
let lastName: string = "山田";

// 連結
let fullName: string = firstName + " " + lastName;

// 出力
console.log(fullName); // 太郎 山田
```

ここでは、2つの文字列を `+` 演算子で結合し、新しい文字列を作成しています。また、空白を入れることによって、`太郎` と `山田` の間にスペースが入るようになっています。

## 深堀り

より詳細な連結の手法について深堀りを行います。TypeScript では、文字列を連結する他にも、`concat()` メソッドを使用することもできます。これは、文字列の配列を一つの文字列に結合することができるメソッドです。

```TypeScript
// 文字列の配列の宣言
let fruits: string[] = ["りんご", "バナナ", "みかん"];

// 連結
let fruitsString: string = fruits.concat();

// 出力
console.log(fruitsString); // りんごバナナみかん
```

また、連結した文字列を変数に代入せずに、直接 `console.log()` する方法もあります。

```TypeScript
// 文字列の宣言
let bearName: string = "ブラウン";
let bearFriend: string = "サリー";

// 連結と出力を同時に行う
console.log("私たちのクマの友達は " + bearName + " と " + bearFriend + " です。"); // 私たちのクマの友達は ブラウン と サリー です。
```

## See Also

- [TypeScript 公式ドキュメント](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [MDN Web Docs - 文字列の連結](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Array/concat#k-pseudo-depth-connect-languages)