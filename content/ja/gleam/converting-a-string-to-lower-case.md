---
title:                "文字列を小文字に変換する"
html_title:           "Gleam: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

何してるの？
文字列を小文字に変換するとは、プログラマーがやめる理由そのものである。一般的に、文字列を比較する際に大文字と小文字を区別しないために使用されます。

手順：
```
Gleam.String.to_lower("Hello") // "hello"
Gleam.String.to_lower("WORLD") // "world"
Gleam.String.to_lower("") // ""
```

深く掘り下げる：
この方法は、一般的にデータの処理や文字列の比較などに使用されてきました。そして、大文字と小文字を区別しないことは、多くの言語にとって基本的な機能であるため、さまざまな言語で同じような機能を見つけることができます。実装にあたって、様々なアルゴリズムやデータ構造を使用することにより、文字列を効率的に変換することができます。

参考:
- [GleamのStringモジュール](https://gleam.run/modules/gleam/string.html)
- [他の言語でも同様の機能を使用する方法](https://qiita.com/tomasu/items/c4338798bd52f7feee75)
- [大文字・小文字を区別しない比較の実装方法](https://github.com/microsoft/CodeContracts/issues/319)