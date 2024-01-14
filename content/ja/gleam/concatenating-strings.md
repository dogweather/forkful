---
title:    "Gleam: 文字列の連結"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結を行う理由は、プログラミングにおいてよく使用される一つの基本的なタスクです。文字列は、プログラム内でデータを表現するために使用されるため、複数の文字列を効率的に結合することは非常に重要です。

## 方法

文字列の連結を行う方法はいくつかありますが、Gleamでは "+" 演算子を使用して簡単に実装することができます。例えば、文字列 "Hello" と "World" を連結するには、次のようにコードを書きます。

```Gleam
"Hello" + "World"
```

これにより、"HelloWorld" という出力が得られます。また、文字列の変数を使用して連結することもできます。

```Gleam
name = "Gleam"
"Hello, " + name
```

この場合、出力は "Hello, Gleam" となります。

## ディープダイブ

Gleamでは、文字列のマイクロ言語を使用して文字列を連結することもできます。これは、より複雑な文字列の操作を行う場合に便利です。例えば、"Hello World" という文字列を "Hello, World!" に変更するには、次のようにコードを書きます。

```Gleam
string = "Hello {name}"
string = string.append("!")
```

これにより、変数 "{name}" が "Gleam" で置換され、"Hello, Gleam!" という出力が得られます。

## 関連情報

[Gleamドキュメンテーション](https://gleam.run/documentation)  
[Gleam GitHubリポジトリ](https://github.com/gleam-lang/gleam)  
[Gleamコミュニティチャット](https://github.com/gleam-lang/gleam/discussions)