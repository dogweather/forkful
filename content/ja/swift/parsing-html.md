---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/parsing-html.md"
---

{{< edit_this_page >}}

# HTMLパースとは何ですか?なぜプログラマーはそれを行うのですか?
HTMLパースは、HTMLコードの構造と内容を理解するためのプロセスです。HTMLをパースすることで、プログラマーはWebコンテンツを抽出、操作、そして分析することができます。

# どうやって:
Swiftを使用してHTMLをパースする基本的な方法は次のとおりです。以下にサンプルコードとその出力も示します。

```Swift
import Foundation
import SwiftSoup

var html = "<html><head></head><body><h1>Hello, world!</h1></body></html>"

do {
    let doc: Document = try SwiftSoup.parse(html)
    let h1: Element = try doc.select("h1").first()!
    print(try h1.text())
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("error")
}
```
このコードは"Hello, world!"と出力します。

# 深い潜入:
HTMLパースの技術は、1990年代の初めにWorld Wide Webの誕生とともに登場しました。初期のHTMLパーサーは単純で制限がありましたが、時間と共にその技術は進化し、より複雑な操作が可能になりました。

Swiftには、他にもHTMLをパースする方法があります。たとえば、`XMLParser`や`NSHTMLTextDocumentType`といったクラスを使用することも可能です。

SwiftSoupというライブラリを使用したHTMLパースは操作が容易で、使いやすいインターフェースを提供しています。SwiftSoupはJavaのJsoupライブラリをSwiftに移植したもので、その威力はJavaと同等です。

# 関連参照:
- [SwiftSoup公式ドキュメント](https://scinfu.github.io/SwiftSoup/)
- [Appleの公式XMLParserドキュメント](https://developer.apple.com/documentation/foundation/xmlparser)
- [WWDC 2017-Session 719 'Working with Web Data'](https://developer.apple.com/videos/play/wwdc2017/719/)