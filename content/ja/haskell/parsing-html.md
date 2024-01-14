---
title:                "Haskell: HTMLのパース"
simple_title:         "HTMLのパース"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLのパージングを行うのに、なぜ時間を費やすのでしょうか？HTMLはウェブ上で最も一般的な言語であり、Haskellでプログラミングする上で、ウェブスクレイピングにおいて非常に重要なスキルです。また、HTMLを解析することで、データを抽出したり、ウェブページを操作したりすることができます。

## ハウトゥー

まず最初に、HaskellでHTMLをパースするための基本的な方法を見ていきましょう。ここでは、"tagsoup"と呼ばれるHaskellのHTMLパーサーライブラリを使用します。まずは、このライブラリをインポートします。

```Haskell
import Text.HTML.TagSoup
```

次に、HTMLページのURLを指定して、"openURL"を使用してHTMLコンテンツを取得します。そして、取得したコンテンツを"parseTags"を使用してタグのリストに変換します。

```Haskell
html <- openURL "https://www.example.com/"
let tags = parseTags html
```

これで、HTMLページをタグのリストとして取得することができました。次に、"tagsoup"ライブラリには便利な関数がいくつかあります。例えば、"~>"を使用すると、特定のタグの間のテキストを取得することができます。

```Haskell
let title = fromTagText $ takeWhile (~/= "</title>") $ dropWhile (~/= "<title>") tags
print title
```

上記のコードでは、タイトルタグの中身を取得し、プリントすることができます。このように、"tagsoup"ライブラリを活用することで、簡単にHTMLコンテンツをパースすることができます。

## ディープダイブ

さらに、より複雑なHTMLのパージングについて説明します。HTMLには多くのタグがあり、それぞれに異なる属性があるため、すべてのHTMLをパーズするのは簡単ではありません。しかし、"tagsoup"ライブラリには様々な関数が用意されており、特定のタグや属性を指定してHTMLをパーズすることができます。

例えば、"onlyKeepTags"を使用すると、指定したタグ以外のタグを削除することができます。

```Haskell
let tags = parseTags html
let filteredTags = onlyKeepTags ["p", "h1", "img", "a"] tags
```

また、"stringToTag"を使用すると、テキストを指定したタグで囲むことができます。これは、HTMLフォーマットを作成する際に便利です。

```Haskell
let formattedHTML = stringToTag "p" "This text will be wrapped in a paragraph tag."
```

このように、"tagsoup"ライブラリを使用することで、より複雑なHTMLのパージングが可能になります。

## さらに参考になるリンク

- [Haskellのウェブスクレイピング入門](https://dev.to/allenhaltmaier/web-scraping-with-haskell-an-introduction-52l7)
- [tagsoupライブラリのドキュメント](https://hackage.haskell.org/package/tagsoup)
- [HaskellでのHTMLパーシングのチュートリアル](https://www.schoolofhaskell.com/user/commercial/content/parsing-html)