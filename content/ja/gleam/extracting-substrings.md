---
title:                "Gleam: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# サブストリングを抽出する理由

サブストリングの抽出にはいくつかの理由があります。たとえば、テキストを処理する際に、特定の単語やフレーズを抽出したい場合があります。また、文字列の検索や置換を行うときにもサブストリングを抽出することが役に立ちます。Gleamプログラミング言語では、文字列を効率的に処理するための便利な関数が用意されています。

# サブストリングの抽出方法

サブストリングを抽出するには、Gleamの`String.substring(start, end, string)`関数を使用します。例えば、次のコードでは、文字列の最初から5文字目までのサブストリングを抽出しています。

```
Gleam.import String

let example_string = "こんにちは、Gleamです。"

String.substring(0, 5, example_string)
// Output: "こんにちは"
```

また、`String.indexOf(substring, string)`関数を使用すると、指定した文字列が含まれる位置を見つけることができます。例えば、次のコードでは、"Gleam"という文字列が含まれる位置を抽出しています。

```
Gleam.import String

let example_string = "こんにちは、Gleamです。"

String.indexOf("Gleam", example_string)
// Output: 6
```

# サブストリングの詳細

サブストリングを抽出する際には、抽出する文字列の開始位置と終了位置を指定する必要があります。開始位置は0から始まるインデックスで指定しますが、終了位置は含まれないことに注意してください。つまり、最初の文字は0番目のインデックスに対応し、2番目の文字は1番目のインデックスに対応します。また、`String.substring(start, end, string)`関数の第三引数には、抽出する元の文字列を指定します。

# 参考リンク

- Gleam公式ドキュメント (https://gleam.run/documentation/)
- GleamのStringモジュールに関するチュートリアル (https://gleam.run/tutorials/string-manipulation/)
- 実践的なGleamプログラミング入門 (https://dev.to/jessewinston/practical-introduction-to-gleam-programming-57df)