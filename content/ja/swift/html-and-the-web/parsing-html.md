---
date: 2024-01-20 15:34:33.144115-07:00
description: "How to: (\u3084\u308A\u65B9) Swift\u3067HTML\u3092\u30D1\u30FC\u30B9\
  \u3059\u308B\u306B\u306F\u3001\u7B2C\u4E09\u8005\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\
  \u529B\u3092\u501F\u308A\u308B\u306E\u304C\u4E00\u822C\u7684\u3067\u3059\u3002 `SwiftSoup`\
  \ \u304C\u30DD\u30D4\u30E5\u30E9\u30FC\u306A\u9078\u629E\u80A2\u306E\u4E00\u3064\
  \u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001SwiftSoup\u3092\u4F7F\u7528\u3057\u3066\
  HTML\u304B\u3089\u30BF\u30A4\u30C8\u30EB\u3092\u53D6\u5F97\u3059\u308B\u30B7\u30F3\
  \u30D7\u30EB\u306A\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.610345-06:00'
model: unknown
summary: "Swift\u3067HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u306B\u306F\u3001\u7B2C\
  \u4E09\u8005\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u529B\u3092\u501F\u308A\u308B\u306E\
  \u304C\u4E00\u822C\u7684\u3067\u3059."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## How to: (やり方)
SwiftでHTMLをパースするには、第三者ライブラリの力を借りるのが一般的です。 `SwiftSoup` がポピュラーな選択肢の一つです。以下は、SwiftSoupを使用してHTMLからタイトルを取得するシンプルな例です。

```Swift
import SwiftSoup

let htmlString = "<html><head><title>おはよう、世界！</title></head><body></body></html>"

do {
    let doc: Document = try SwiftSoup.parse(htmlString)
    if let title = try doc.title() {
        print(title) // おはよう、世界！
    }
} catch Exception.Error(let type, let message) {
    print("Type: \(type)")
    print("Message: \(message)")
} catch {
    print("error")
}
```

このコードはHTML文字列を解析し、`<title>`タグ内のテキストを取得して出力します。

## Deep Dive (深掘り)
HTMLパーシングはWebの始まりから重要な役割を果たしてきました。古くは正規表現を使っていましたが、エラーが起きやすく安定性に欠けていました。だから専門のパーサーが誕生しました。これらはHTMLの階層構造を理解し、堅牢かつ正確にデータを取り出すことができます。

Swiftにおける代わりとしては、`NSXMLParser`を用いるのがありますが、HTMLが常にきちんと形式化されている保証はないため、XMLパーサーには向かないことが多いです。`SwiftSoup`のようなHTML専用のパーサーは、不整合なマークアップを上手く扱えるのが利点です。

実装面においては、パーサーは内部的にDOM(Document Object Model)ツリーを構築し、操作やデータの抽出を行います。パフォーマンスやメモリ使用に関する考慮も重要です。大規模なHTMLドキュメントを扱う場合、効率的に処理することが挑戦となります。

## See Also (関連情報源)
- SwiftSoup GitHubリポジトリ：[https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- AppleのNSXML解析器ドキュメント：[https://developer.apple.com/documentation/foundation/nsxmlparser](https://developer.apple.com/documentation/foundation/nsxmlparser)
