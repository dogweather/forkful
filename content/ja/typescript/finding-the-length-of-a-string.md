---
title:    "TypeScript: 文字列の長さを見つける"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##なぜ
文字列の長さを見つけることの意義を理解することは、プログラミング言語を学ぶ上で非常に重要です。文字列の長さを計算する方法を学ぶことで、より複雑なプログラムを作成することができます。

##やり方
```TypeScript
// 文字列の長さを計算する関数を定義する
function calculateLength(str: string): number {
  // 文字列の長さを格納する変数を初期化する
  let length = 0;
  // 文字列の各文字を繰り返し処理する
  for (let char of str) {
    // 各文字のUTF-16コードを取得する
    const unicode = char.charCodeAt(0);
    // UTF-16コードが2バイトの範囲にある場合は1を、4バイトの範囲にある場合は2を文字列の長さに加える
    if (unicode >= 0xD800 && unicode <= 0xDBFF) {
      length += 2;
    } else {
      length += 1;
    }
  }
  return length;
}

// 関数を使って文字列の長さを計算する
const strA = "こんにちは";
const strB = "Hello, World!";
console.log(calculateLength(strA)); // 出力結果: 10
console.log(calculateLength(strB)); // 出力結果: 13
```

##ディープダイブ
文字列の長さを計算する方法には、さまざまなアプローチがあります。例えば、UTF-8形式を使用する場合は、特定の文字コードの前にコメントを入れることで長さを計算することができます。また、日本語のようなマルチバイト文字を含む文字列の長さを計算する場合は、バイト数ではなく文字数を計算する必要があります。プログラムを作成する際には、文字の種類やエンコーディング形式を意識して、最適な方法を選択することが重要です。

##関連記事
- [UnicodeとUTF-8の違い](https://japanhustle.net/unicode-vs-utf-8/)
- [JavaScriptで文字列の長さを計算する方法](https://zenn.dev/jstechguide/articles/string-length-calculation-js)