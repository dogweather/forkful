---
title:                "部分文字列の抽出"
aliases:
- /ja/javascript/extracting-substrings.md
date:                  2024-01-20T17:46:25.671922-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
JavaScriptでは、文字列から特定の部分を取り出す操作を「サブストリング（部分文字列）の抽出」と呼びます。この技術は、ユーザー入力の処理やデータの形式を変更する際に頻繁に使われます。

## How to: (方法)
JavaScriptでサブストリングを抽出する主な方法は、`substring()`, `slice()`, そして `substr()` の3つです。以下のサンプルコードを見てみましょう。

```javascript
let text = "こんにちは、JavaScript!";

// substring()を使用
let sub1 = text.substring(6, 16);
console.log(sub1); // 出力: JavaScript

// slice()を使用
let sub2 = text.slice(6, 16);
console.log(sub2); // 出力: JavaScript

// substr()を使用（非推奨）
let sub3 = text.substr(6, 10);
console.log(sub3); // 出力: JavaScript
```

## Deep Dive (掘り下げ)
`substring()` と `slice()` は似ていますが、負のインデックスを扱う場合が異なります。`slice()` は負のインデックスをサポートし、文字列の末尾からの位置を参照します。一方、`substring()` は負の値を0として扱います。`substr()` は現在、非推奨とされ、将来的には削除される可能性があります。これらのメソッドはECMAScript規格の進化に伴い追加され、より柔軟に文字列処理を行えるようになりました。しかし、可読性や未来のコード互換性を考えると、`slice()` がより優れた選択肢と言えます。

## See Also (関連情報)
- MDN Web Docsによる`String.prototype.substring()`の解説: [MDN substring](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- MDN Web Docsによる`String.prototype.slice()`の解説: [MDN slice](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- MDN Web Docsでの`String.prototype.substr()`の非推奨についての議論: [MDN substr](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
