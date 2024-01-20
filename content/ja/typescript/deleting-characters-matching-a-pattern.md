---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

パターンにマッチする文字を削除するとは、特定のパターンに一致する文字を文字列から取り除く操作を指します。これは、不要なスペースや特殊文字を除去し、データクリーニングプロセスを支援するため、プログラマーが頻繁に行う作業です。

## どうする：

TypeScriptにおいて文字列から特定のマッチする文字（パターン）を削除する例を見てみましょう。

```TypeScript
let text = "TypeScript が大好きです！";
let pattern = /が|！/g; // "が" または "！" のパターン
let newText = text.replace(pattern, ''); // パターンにマッチする文字を削除
console.log(newText); // "TypeScript 大好きです"
```

この例では、文字列 "TypeScript が大好きです！" から "が" と "！" を削除しています。`replace`関数がパターンにマッチする文字を削除します。

## ディープダイブ

歴史的なコンテクストから見ると、文字列から特定のパターンを削除するこのような操作は、Perlで使われていた正規表現が原点であり、これが後のプログラミング言語に影響を与えました。

代替法として、`filter`関数や`split`と`join`を使用する方法などが考えられますが、`replace`関数を使用する方法は、一致する文字が存在する場所を気にせず、簡単に文字列を変更できるため、非常に便利です。

実装の詳細については、`replace`関数は正規表現に基づいて文字列内のパターンを検索し、第二引数の文字列に置換することで動作します。この正規表現のパターンマッチング能力は、この操作を強力かつ柔軟性が高いものにしています。

## 参照

以下は、このトピックに関連する役立つソースへのリンクです：

- Mozilla デベロッパーネットワークの `replace()` 関数のドキュメント: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- 正規表現についての詳しい情報：https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions
- TypeScript の公式ドキュメンテーション：https://www.typescriptlang.org/docs/