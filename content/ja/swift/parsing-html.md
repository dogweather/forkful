---
title:                "Swift: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/parsing-html.md"
---

{{< edit_this_page >}}

## なぜHTMLをパースする必要があるのか

HTMLはウェブページを表示するための言語ですが、その中に必要なデータが含まれていることもあります。そのため、HTMLをパースすることで必要な情報を抽出することができます。例えば、ウェブスクレイピングやデータ収集などに役立ちます。

## HTMLをパースする方法

まずはSwiftのHTMLパーサーライブラリであるSwiftSoupをインストールします。次に、```Swift import SwiftSoup```というコードを使ってSwiftSoupをインポートします。HTMLのURLを指定すると、以下のコードを使ってHTMLをパースできます。

```Swift
let html = try String(contentsOf: URL(string: "https://www.example.com")!)
let doc: Document = try SwiftSoup.parse(html)
```

パース後、必要な要素をCSSセレクターを使って取得することができます。例えば、```doc.select("h1")```というコードを使うことで、HTML内のすべての```h1```タグを取得することができます。

## HTMLパースのさらなる探求

HTMLパースには、CSSセレクター以外にもXPathや正規表現を使う方法があります。また、SwiftSoup以外にもHTMLパーサーライブラリは存在します。パースの方法やライブラリを使いこなせるようになると、より高度なデータ収集や処理が可能になります。

## See Also

- [SwiftSoup公式ドキュメント](https://github.com/scinfu/SwiftSoup/wiki)
- [HTMLパースについての詳しい解説 (英語)](https://www.smashingmagazine.com/2019/12/complete-guide-parsing-html-swift/)
- [XPathの使い方 (日本語)](https://qiita.com/2nd_Jeunese/items/99897e00c9933be6e654)
- [正規表現の使い方 (日本語)](https://www.sejuku.net/blog/11630)