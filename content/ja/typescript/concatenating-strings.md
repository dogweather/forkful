---
title:    "TypeScript: 文字列の連結"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# なぜ ？

文字列を連結することは、プログラミングで頻繁に使用される基本的なタスクです。例えば、ウェブアプリケーションがデータベースから取得した情報を表示する際に、文字列を結合して希望の形式で表示する必要があります。TypeScriptでは、文字列の結合をより簡単に行うことができます。

## 使い方

「+」演算子を使用して、文字列を結合することができます。例えば、"Hello "という文字列と"World"という文字列を結合したい場合、以下のようにコードを書きます。

```TypeScript
let greeting: string = "Hello ";
let name: string = "World";

let message: string = greeting + name;
console.log(message);
```

この場合、コンソールには"Hello World"という文字列が出力されます。

また、テンプレート文字列を使用することもできます。テンプレート文字列を使用すると、変数を埋め込んで文字列を結合することができます。同じ例をテンプレート文字列を使用して書くと、以下のようになります。

```TypeScript
let greeting: string = "Hello ";
let name: string = "World";

let message: string = `${greeting}${name}`;
console.log(message);
```

## ディープダイブ

文字列の結合は、プログラミングの世界で非常によく使用される概念です。しかし、実際にはコードを実行する前に、TypeScriptでは文字列の結合をどのように処理しているのか、また文字列を結合する際のパフォーマンスについて知ることも重要です。

TypeScriptでは、文字列の結合の際に、内部的にはStringBuilderというものを使用しています。これは、パフォーマンスの観点から最適化された方法で文字列を結合するための仕組みです。

また、文字列の結合には+演算子の代わりにconcat()メソッドを使用することもできます。このメソッドは、複数の文字列を結合する際にも効率的に動作します。しかし、通常は+演算子を使用する方がコードがより読みやすくなります。

## おすすめのリンク

[TypeScript公式サイト](https://www.typescriptlang.org/)<br>
[MDN - テンプレート文字列](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/template_strings)<br>
[理解できるTypeScript入門 - 文字列の結合](https://www.sejuku.net/blog/78583)