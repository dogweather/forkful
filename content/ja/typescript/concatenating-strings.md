---
title:                "文字列の連結"
html_title:           "TypeScript: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

「## 何か？」
文字列を結合するとは、単純に文字列同士をつなげることを意味します。プログラマーは、プログラム内で動的なテキストを作成するために、このような文字列を結合する必要があります。

「## 方法：」
TypeScriptで文字列を結合する方法はいくつかあります。最も基本的な方法は、"+"記号を使用することです。例えば、"Hello"という文字列と"World"という文字列を結合したい場合は、次のように書きます。
```
TypeScript
let greeting: string = "Hello";
let name: string = "World";
console.log(greeting + " " + name);
```
出力結果は「Hello World」となります。

別の方法として、テンプレート文字列を使用することもできます。テンプレート文字列を使用すると、文字列内に変数を直接埋め込むことができます。上記の例をテンプレート文字列で書くと、次のようになります。
```
TypeScript
let greeting: string = "Hello";
let name: string = "World";
console.log(`${greeting} ${name}`);
```
出力結果は同じく「Hello World」となります。

「## 詳細を掘り下げる」
文字列の結合はプログラミングの歴史の中で重要な役割を果たしてきました。早い時期から、文字列を結合するためのさまざまな方法が考案され、改良されてきました。例えば、JavaのStringクラスでは、文字列結合のための「+」演算子が導入されました。

また、文字列の結合には他にもいくつかの代替手段があります。例えば、文字列を結合するときには文字列結合関数を使用することもできます。これは、ある文字列と別の文字列を受け取り、両方を結合した新しい文字列を返す関数です。

文字列の結合は、とても基本的でありながら、プログラミングで重要な役割を果たす概念です。そのため、慣れておくことが大切です。

「## 関連情報を参照」
- [JavaScriptの文字列結合方法（W3Schools）](https://www.w3schools.com/js/js_string_concat.asp)
- [JavaのStringクラスのドキュメント（Oracle）](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
- [C#での文字列結合方法（Microsoft）](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/addition-operator)