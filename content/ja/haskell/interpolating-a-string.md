---
title:                "文字列の補間"
html_title:           "Haskell: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何が？　どうして？

文字列を補間(interpolate)することは、Haskellプログラミングにおいて、文字列の一部を変数や式の値で置き換えることを意味します。プログラマーたちはこのようなことをすることで、コードをより簡潔で読みやすく、メンテナンスしやすくすることができます。

## 方法：

文字列を補間するには、テンプレートリテラルを使うことができます。下記のように、文字列の前後にバッククォートを使用し、変数や式を`${}`で囲んで置き換えます。

```Haskell
let name = "John"
let age = 30
let greeting = `Hello, my name is ${name} and I am ${age} years old.`
```

これにより、`greeting`変数には`"Hello, my name is John and I am 30 years old."`という文字列が補間されます。

## 深堀：

補間するというアイデアは、もともとインタプレタ言語であるプログラミング言語から来ています。この方法は簡単であるため、多くの言語がこの機能を実装しています。Haskellにおいては、補間はコードをより簡潔で可読性が高くなるため、非常に便利です。また、バッククォートを使用することで、変数や式を識別しやすくなります。

## さらに見る：

- [Haskellで文字列を連結する方法](https://qiita.com/kshina76/items/0a65254b6381ceacb34a)
- [Haskellのテンプレートリテラルについて](https://wiki.haskell.org/Template_Haskell)
- [Haskellにおける文字列操作の詳細](https://wiki.haskell.org/Strings)