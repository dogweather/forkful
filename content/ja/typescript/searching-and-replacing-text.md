---
title:                "TypeScript: テキストの検索と置き換え"
simple_title:         "テキストの検索と置き換え"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ検索と置換を行うのか

テキストを編集する際には、しばしば同じ単語やフレーズを複数箇所に使用したり、古いテキストを新しいものに置き換えたりする必要があります。そのような場合、手動で行うと時間がかかり、ミスも起こりやすくなります。しかし、TypeScriptを使用することで、より効率的に検索と置換を行うことができます。

## 方法

検索と置換を行うには、まず`replace()`メソッドを使用して対象の文字列を置き換える必要があります。例えば、`"I like TypeScript".replace("TypeScript", "programming")`とすることで、"TypeScript"を"programming"に置き換えることができます。

また、正規表現を使用することで、より複雑なパターンでの検索と置換が可能になります。`/[0-9]+/.replace("1, 2, 3", "数字")`とすることで、数字を含む文字列を"数字"に置き換えることができます。

```TypeScript
"I like TypeScript".replace("TypeScript", "programming")
// output: "I like programming"

/[0-9]+/.replace("1, 2, 3", "数字")
// output: "数字, 数字, 数字"
```

## 深堀り

TypeScriptの`replace()`メソッドは、文字列だけでなく、正規表現オブジェクトも受け入れることができます。これにより、より高度な検索と置換を行うことができます。

また、`replace()`メソッドはデフォルトでは最初にマッチした箇所のみを置き換えるため、全てのマッチ箇所を置換する場合は`g`(global)フラグを使用する必要があります。

さらに、`replace()`メソッドは第二引数に関数を渡すこともできます。これにより、置換箇所ごとに処理を行うことができます。

```TypeScript
"Learn TypeScript and become a better programmer".replace(/(TypeScript|programmer)/g, (match) => {
  if (match === "TypeScript") {
    return "JavaScript";
  } else {
    return "developer";
  }
});

// output: "Learn JavaScript and become a better developer"
```

## 参考リンク

- [TypeScript String Replace Method](https://www.tutorialspoint.com/typescript/string_replace.htm)
- [JavaScript Standard Built-in Objects - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [正規表現チートシート](https://dev.to/pstrinkle/javascript-regular-expressions-cheat-sheet-2j2a)
- [TypeScript Handbook - Template Literal Types](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#template-literal-types)

## 参照

[これから始めるTypeScript入門 - 第6回 検索／置換 (前編)](https://codezine.jp/article/detail/12540)

[JavaScriptのreplace()メソッドの使い方の解説](https://techacademy.jp/magazine/12249)