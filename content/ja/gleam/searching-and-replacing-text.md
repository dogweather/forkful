---
title:                "テキストの検索と置換"
html_title:           "Gleam: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

*こんにちは、Gleamプログラマーのみなさん！今日は、我々がよく行うテキストの検索と置換についてお話ししましょう。*

## What & Why?

テキストの検索と置換とは、コンピューターのプログラムでよく使われる作業です。文書やコードの中に特定の文字列を探して、別の文字列に置き換えることを意味します。プログラマーがこれをする理由は、大量のテキストを手作業で変更するのは非効率的であり、プログラムを使って自動化する方が速く正確であるためです。

## How to:

```Gleam
import gleam/string

// 指定した文字列を別の文字列に置き換える
gleam/string.replace("Hello, world!", "world", "Gleam") // => "Hello, Gleam!"

// 正規表現を使って複雑なパターンを置き換える
gleam/string.replace_regex("My phone number is (123)456-7890", "[0-9]", "*") // => "My phone number is (***)***-****"
```

## Deep Dive:

テキストの検索と置換は、コンピューターが広く使われるようになってから必須の作業となりました。以前は手作業で行われていたものを、プログラミング言語やツールを使って自動化することで、開発効率が格段に上がりました。代表的な方法として正規表現がありますが、パターンを覚えるのが難しく、プログラマーにとっては一つの技術として習得するのに時間がかかります。他にも、テキストの検索と置換を行うために特化したツールもありますが、プロジェクトによってはプログラムの一部として実装する必要があります。

## See Also:

- [Gleamの公式ドキュメント](https://gleam.run/)
- [正規表現をマスターする30分](https://medium.com/@carolchang/30%E5%88%86%E9%96%93%E3%81%A7%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE%E3%82%92%E3%83%9E%E3%82%B9%E3%82%BF%E3%83%BC%E3%81%99%E3%82%8B-8931c6cd408f)