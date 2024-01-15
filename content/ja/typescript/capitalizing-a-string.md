---
title:                "「文字列のキャピタライズ」"
html_title:           "TypeScript: 「文字列のキャピタライズ」"
simple_title:         "「文字列のキャピタライズ」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##なぜ

文字列を大文字にすることで何ができるのか疑問に思ったことはありませんか？大文字の文字列は、プログラミングにおいても多くの場面で必要とされます。例えば、ユーザーが入力した文字列を正規化したり、データベース内での検索を簡単にしたりするために使われます。今回は、TypeScriptを使って文字列を大文字にする方法を紹介します。

##やり方

まずは、大文字にしたい文字列を用意しましょう。例として、"hello world"という文字列を大文字に変換したいとします。

```TypeScript
let str = "hello world";
let capitalizedStr = str.toUpperCase();
console.log(capitalizedStr);
```

上記のコードを実行すると、"HELLO WORLD"という結果が得られます。まず、toUpperCase()というメソッドを使うことで、文字列の全ての文字を大文字に変換することができます。その後、変数capitalizedStrに結果を格納し、console.log()を使って結果を出力しています。

もし文字列がすでに大文字の場合は、何も変化しません。また、このメソッドは元の文字列を変更するのではなく、新しい文字列を返します。そのため、元の文字列を保持したまま大文字に変換することができます。

##ディープダイブ

今回使ったtoUpperCase()メソッドはStringクラスに属しており、文字列に対して使うことができるメソッドの一つです。Stringクラスには他にも、文字列を別の文字列で置き換えるreplace()メソッドや文字列を分割するsplit()メソッドなど、便利なメソッドが多数あります。

また、TypeScriptでは文字列の型付けが厳密に行われるため、バグを防いだり安全性の高いプログラミングを行うことができます。文字列を扱う際は、型付けにも注意しましょう。

##参考リンク

- [String - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String)
- [TypeScript: String Operations](https://www.typescriptlang.org/docs/handbook/strings.html)
- [String.prototype.toUpperCase() - JavaScript | MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)

##もっと学ぼう

今回は文字列を大文字に変換する方法を紹介しましたが、実際のプロジェクトではさらに多くの文字列操作が必要になるかもしれません。ぜひ、参考リンクを読んでさらに知識を深めてみてください。また、TypeScriptの公式ドキュメントもぜひチェックしてみてください。