---
title:                "文字列を大文字にする"
html_title:           "TypeScript: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の先頭を大文字にするとは、文字列内の各単語の最初の文字を大文字にする操作を指します。プログラマがこれを行う主要な理由は、ユーザーが読みやすい形式でデータを表示することです。

##方法：

TypeScriptで文字列の先頭を大文字にする基本的な方法は以下のようになります:

```TypeScript
function capitalize(s: string): string {
    return s.charAt(0).toUpperCase() + s.slice(1);
}

console.log(capitalize('typeScript article')); // Output: 'TypeScript article'
```

次に、型安全な方式を示します。型チェックが追加され、引数が文字列であることが保証されます：

```TypeScript
type StringFunction = (s: string) => string;

const safeCapitalize: StringFunction = (s) => s.charAt(0).toUpperCase() + s.slice(1);

console.log(safeCapitalize('typeScript tutorial')); // Output: 'TypeScript tutorial'
```

##深掘り：

文字列の最初を大文字にする技術は古くからありますが、この考え方の一部は、古代の手書き文書に由来していると言われています。ここでは、新しいセクションの最初の文字がしばしば大きく、装飾的に書かれていました。

また、大文字変換の代替手段としては、CSSを使用したスタイル変化もあります。しかし、これは表示のみを変更し、文字列そのものを変更するわけではないことに注意が必要です。

TypeScriptの実装では、`toUpperCase`関数と`charAt`関数を使用して最初の文字を取得し、大文字に変換します。その後、`slice`関数を使用して元の文字列の残りの部分を取得し、大文字に変換した先頭文字と結合します。

##関連リンク：

TypeScriptの他の要素との連携については、以下のリンクが参考になるでしょう。

- [TypeScriptの公式文書](https://www.typescriptlang.org/docs/)
- [JavaScriptでの文字列操作](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)
- [TypeScriptとJavaScriptの比較](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)