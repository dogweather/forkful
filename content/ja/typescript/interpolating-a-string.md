---
title:                "文字列の補間"
html_title:           "TypeScript: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## なに？なぜ？
文字列をインターポレーションするのは、文字列中に変数を埋め込むことです。プログラマーがこれを行う理由は、コードを簡潔に保ち、可読性を高めるためです。

## 方法：
```TypeScript
const name = "太郎";
const message = `こんにちは、${name}さん！`;
```
出力：こんにちは、太郎さん！

## ディープダイブ：
(1) インターポレーションが導入された背景
インターポレーションは、ES6以降のJavaScriptで導入されました。これにより、以前は行なえなかった変数を文字列内に埋め込むことができるようになりました。
(2) 代替手段
インターポレーション以外にも、文字列連結やテンプレートリテラルを使用して変数を文字列に組み込むことができます。
(3) 実装の詳細
バッククォート「`」で囲まれた文字列内で、${}を使用することで変数をインターポレーションすることができます。

## 関連リンク
- [MDN web docs - 文字列の基礎](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/string)
- [公式TypeScriptハンドブック](https://www.typescriptlang.org/docs/home.html)