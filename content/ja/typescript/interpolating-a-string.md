---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 文字列の補間とは何ですか? - TypeScriptの視点から見る

## なぜ&何のために？

文字列の補間（String Interpolation）は、文字列の中に変数や式を埋め込む処理を指します。プログラマーがこれを行う理由は、動的な文字列の作成が容易になるからです。

## どうやって行いますか？

TypeScriptではバッククオート(``)を用いて、変数や式を${...}内に書くことで文字列補間を行えます。

以下に例を示します：

```TypeScript
let name = "Yamada";
console.log(`Hello, ${name}`);  
```

出力結果：

```
Hello, Yamada
```
このように、文字列内に直接値を展開することができます。

## 詳細な情報

補間という概念は古いものであり、多くのプログラミング言語で既に採用されています。TypeScript以外にもPerlやRubyなどではすでに使われています。

替わりに、伝統的な連結操作子+を使用することもできますが、補間は可読性と書きやすさで勝ると考えられています。

TypeScriptでは、内部的にはテンプレートリテラルを使用して文字列補間が実装されています。

## 参考になる情報

以下に、文字列補間の詳細な情報についてのリンクを添付します：

1. [MDN Web Docs - テンプレートリテラル](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Template_literals)

2. [Stack Overflow - 文字列の補間(TypeScript)](https://stackoverflow.com/questions/31079081/typescript-string-interpolation)