---
title:                "パターンにマッチする文字を削除する"
html_title:           "TypeScript: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何を & なぜ？
パターンに一致する文字を削除するとは、文字列の中から特定の規則に基づいて特定の文字を削除することを指します。プログラマーは、文字列の中から特定の文字を削除することで、文字列をより簡単に処理することができます。

## 方法：
```TypeScript
const str: string = "Hello World!";
const pattern: RegExp = /l/g;
const newStr = str.replace(pattern, "");
console.log(newStr);
// Output: Heo Word!
```

## 深く掘り下げる：
1. 歴史的背景：パターンに一致する文字を削除するという機能は、古くからプログラミング言語に組み込まれていましたが、TypeScriptでは正規表現を使用することでより柔軟に処理することができます。
2. 代替方法：パターンに一致する文字を削除する代替方法としては、文字列を分割して配列に格納し、不要な文字を取り除いた後に再度結合する方法などがあります。
3. 実装の詳細：TypeScriptでは、正規表現を使用して文字列を検索し、一致する部分を削除することでパターンに一致する文字を削除しています。

## 関連リンク：
- [MDN web docs: RegExp](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Wikipedia: パターンマッチング](https://ja.wikipedia.org/wiki/%E3%83%91%E3%82%BF%E3%83%BC%E3%83%B3%E3%83%9E%E3%83%83%E3%83%81%E3%83%B3%E3%82%B0)