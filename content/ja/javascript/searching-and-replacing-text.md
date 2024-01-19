---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なんで、そしてなぜ?
テキストの検索と置換は、あるテキスト内の特定の文字列を探し、新しいテキストに置き換えることです。これは、データのバリデーション、エラーのデバッグ、データの更新など、プログラミングの多くの側面で重要であり、効率的なコーディングを実現します。

## 使い方
この機能はJavascriptの `replaceAll()`メソッドを使うことで実現できます。例えば:

```Javascript
let str = "こんにちは、世界！世界は美しいです。";
let newStr = str.replaceAll("世界", "地球");
console.log(newStr);
```
出力: 
```Javascript
"こんにちは、地球！地球は美しいです。"
```

## ディープダイブ
テキストの探索と置換は早くから存在していました。Javascriptでは、最初に `replace()` メソッドが提供されましたが、これは初めて出現する文字列だけを置換しました。その後、 `replaceAll()` メソッドが導入され、すべての一致する文字列を置換できるようになりました。

`replace()` または正規表現と `g` フラグ（全体マッチ）を使用することで `replaceAll()` と同じ結果が得られますが、`replaceAll()` の方が直感的で分かりやすいです。

```Javascript
let str = "こんにちは、世界！世界は美しいです。";
let newStr = str.replace(/世界/g, "地球");
console.log(newStr);
```
出力:
```Javascript
"こんにちは、地球！地球は美しいです。";
```

## 参考資料
- [MDN Web Docs: replaceAll()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)