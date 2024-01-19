---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結は、複数の文字列を一つに結合するというプログラミング上の操作です。それにより、変動的な情報を表示したり、結果を自由に制御したりします。

## どうやるの？

以下がTypeScriptでの文字列の連結の例です：

```TypeScript
let firstString = "これは ";
let secondString = "お茶です";
let concatString = firstString + secondString;

console.log(concatString);
```

このコードを実行すると、次の出力が得られます：

```
これは お茶です
```

さらに、 ES6以降では、テンプレートリテラル（バックティック ` を使用した文字列）を使ってより簡単に文字列を連結することができます：

```TypeScript
let flavor = "緑";
let concatString = `これは ${flavor} お茶です`;

console.log(concatString);
```

このコードの出力は次のとおりです：

```
これは 緑 お茶です
```

## ディープダイブ

文字列の連結は古くから存在し、その実装や扱いはプログラミング言語によります。TypeScriptでは `+` 演算子を用いて簡易的に連結ができますが、その他にも文字列メソッドやテンプレートリテラルといった手法があります。

文字列の連結は処理速度やメモリ消費に影響を与える可能性があります。大量の文字列を連結する場合は、パフォーマンスを意識して連結方法を選ぶべきです。

また、知っておくべきは、`+` 演算子は数値と文字列の型が混在する場合、暗黙的に型変換を行うという特性がある点です。この挙動は想定外の結果を生む可能性があるため、常に注意が必要です。

## 参考文献

以下は、TypeScriptの文字列連結の詳細について参照できるリンクです：

- [MDN：テンプレートリテラル](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript Deep Dive：文字列](https://typescript-jp.gitbook.io/deep-dive/future-javascript/strings)