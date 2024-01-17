---
title:                "HTMLの解析"
html_title:           "Swift: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/parsing-html.md"
---

{{< edit_this_page >}}

## 何？なぜ？
HTMLのパースとは、ウェブサイトからデータを取得する方法の一つです。プログラマーがHTMLをパースする理由は、ウェブサイトから必要なデータを取得し、処理するためです。

## 方法：
以下は、SwiftでHTMLをパースする簡単なコーディング例です。詳細なコーディングや出力の例は、以下の ```Swift ... ``` コードブロックを参照してください。

```Swift
//HTMLをURLから取得し、パースするためのコード
if let url = URL(string: "https://www.example.com/"){
    do {
        //URLからデータを取得
        let html = try String(contentsOf: url, encoding: .utf8)
        //HTMLパーサーを作成
        let parser = try SwiftSoup.parse(html)
        //必要な要素を取得
        let title = try parser.select("title")
        let paragraph = try parser.select("p")
        //取得した要素を出力
        print(title)
        print(paragraph)
    } catch {
        print("エラー: \(error)")
    }
}
```

## じっくり見る：
パースという概念は、ウェブ技術の最も古い要素の一つです。ウェブサイトからのデータ取得は、プログラマーが手動で行う以前は、自動的にできなかったため、パースが開発されました。代替手段として、スクレイピングやウェブクローリングという方法があります。パースの実装において、一般的に利用されるライブラリには、「SwiftSoup」や「Kanna」などがあります。

## 関連情報：
- 「SwiftSoup」ライブラリのGitHubレポジトリ: https://github.com/scinfu/SwiftSoup
- 「Kanna」ライブラリのGitHubレポジトリ: https://github.com/tid-kijyun/Kanna