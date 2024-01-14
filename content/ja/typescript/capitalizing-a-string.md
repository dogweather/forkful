---
title:    "TypeScript: 文字列の大文字化"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

なぜstringの最初の文字を大文字にするのか？それは、視覚効果を向上させるためです。大文字は重要な単語やタイトルを示すのに適しており、文字列が大きな見出しやエントリポイントを表す場合、よりはっきりと目立つようになります。

## 使い方

```TypeScript
function capitalize(str: string): string {
    // 文字列を小文字に変換
    let lowerCaseStr = str.toLowerCase();
    // 最初の文字を大文字に変換
    let capitalizedStr = lowerCaseStr.charAt(0).toUpperCase() + lowerCaseStr.slice(1);
    return capitalizedStr;
}

console.log(capitalize("typescript")); // TypeScript
console.log(capitalize("今日はいい天気です")); // 今日はいい天気です
```

## 詳細について

文字列を大文字に変換する方法はさまざまありますが、今回はTypeScriptを使用しています。まず、文字列自体を小文字に変換し、その後最初の文字だけを大文字に変換しています。こうすることで、文字列の長さにかかわらず、最初の文字が大文字になります。また、この方法では日本語を含む文字列でも正しく変換することができます。

## See Also

- [String.prototype.charAt() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [String.prototype.toLowerCase() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [String.prototype.toUpperCase() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)