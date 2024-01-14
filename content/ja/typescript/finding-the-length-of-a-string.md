---
title:                "TypeScript: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why (なぜ):

文字列の長さを求めることは、プログラムの中でよく使われる基本的な操作の1つです。文字列の長さを知ることで、処理の範囲を決めたり、文字列を正しく表示したりすることができます。そのため、プログラミングを行う上で非常に役立つスキルです。

## How To (やり方):

TypeScriptを使って文字列の長さを求める方法を紹介します。

まずは、文字列を宣言します。例えば、「Hello TypeScript」という文字列を使います。

```TypeScript
let str: string = "Hello TypeScript";
```

次に、lengthプロパティを使って文字列の長さを求めます。lengthプロパティは、文字列の長さを返す組み込み関数です。

```TypeScript
let strLength: number = str.length;
console.log(strLength);
```

これで、コンソールには「15」という結果が表示されるはずです。なぜなら、スペースも含めて「Hello TypeScript」は15文字からなるからです。

また、Unicodeエスケープシーケンスが含まれる文字列の場合は、lengthプロパティを使うと正しい文字数が返されない場合があります。そのような場合は、StringのfromCharCode関数を使って正しい文字数を求めることができます。

## Deep Dive (深堀り):

文字列のlengthプロパティは、Stringオブジェクトに定義されており、Stringプロトタイプチェーンの一部です。そのため、Stringオブジェクトの機能を継承している点に注意が必要です。

また、lengthプロパティは読み取り専用であり、文字列の長さを変更することはできません。そのため、文字列の長さを求める際には別の変数に値を代入する必要があります。

さらに、バイナリ文字列の場合は、バイト数を返すことに注意が必要です。UTF-16エンコーディングを含んだ文字列の場合は、文字数ではなくUnicodeコードポイントの数を返します。

## See Also (関連情報):

- [TypeScript 公式サイト](https://www.typescriptlang.org/)
- [JavaScriptとは？](https://developer.mozilla.org/ja/docs/Learn/JavaScript/First_steps/What_is_JavaScript)
- [文字列の長さを求める方法](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/length)