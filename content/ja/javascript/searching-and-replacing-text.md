---
title:                "Javascript: テキストの検索と置換"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングでテキストの検索と置換を行う理由は、複数のテキストファイルを処理する際に効率的に作業を行うためです。

## 方法

例えば、あるテキストファイルの中に特定の単語が何度も出現する場合、それらを一括で置換することで作業時間を大幅に短縮することができます。JavaScriptでは、以下のように`replace()`メソッドを使用することで簡単に置換を行うことができます。

```Javascript
let text = "今日はとても暑い日です。";
let newText = text.replace("暑い", "寒い");
console.log(newText); // 出力結果: 今日はとても寒い日です。 
```

このように、`replace()`メソッドを使用することで検索したい単語やフレーズを指定し、その置換する文字列を指定することができます。

## ディープダイブ

さらに、`replace()`メソッドでは正規表現を使用することもできます。これは、検索する際に柔軟性を持たせることができるため、より高度なテキスト検索や置換を行うことができます。例えば、アルファベットの小文字を全て大文字に変換するような置換を行いたい場合、正規表現`/[a-z]/g`を使用することで簡単に実現することができます。

```Javascript
let text = "This is a sample text.";
let newText = text.replace(/[a-z]/g, function(match) {
  return match.toUpperCase();
});
console.log(newText); // 出力結果: THIS IS A SAMPLE TEXT.
```

このように、正規表現を使用することで様々なパターンのテキスト検索や置換を行うことができます。

## 参考リンク

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: 正規表現](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)