---
title:                "ウェブページをダウンロードする"
html_title:           "Swift: ウェブページをダウンロードする"
simple_title:         "ウェブページをダウンロードする"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由はさまざまですが、例えばオフラインで閲覧したい場合や、特定のウェブサイトからデータを収集したい場合には、ウェブページをダウンロードすることが便利です。Swiftを使うと、簡単にウェブページをダウンロードできます。

## ダウンロードの方法

ウェブページをダウンロードするには、まずURLを指定し、そのURLからデータを取得し、そのデータをファイルとして保存する必要があります。以下のコードは、Swiftを使ってウェブページをダウンロードする方法を示しています。

```Swift
// URLを指定
let url = URL(string: "https://www.example.com")!

// データを取得
let data = try! Data(contentsOf: url)

// データをファイルとして保存
FileManager.default.createFile(atPath: "example.html", contents: data, attributes: nil)
```

上記のコードを実行すると、指定したURLからデータを取得し、"example.html"という名前のファイルとして保存されます。

## 深堀り

ウェブページをダウンロードする方法はさまざまありますが、上記の方法は簡単なものです。より高度な方法として、リクエストをカスタマイズしたり、非同期処理を行ったりすることができます。また、ダウンロードしたデータをパースすることで、特定の情報だけを取得することもできます。

## 参考リンク

- [Swift公式サイト](https://swift.org/)
- [Swiftコーディングガイドライン](https://swift.org/documentation/api-design-guidelines/)
- [Swiftでのウェブページのダウンロードの仕方](https://www.hackingwithswift.com/example-code/networking/how-to-download-a-webpage-with-urlsession-and-uitableview)