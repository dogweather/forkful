---
date: 2024-01-20 17:55:26.114929-07:00
description: "How to: (\u3084\u308A\u65B9) \u6700\u521D\u306B\u30D5\u30A1\u30A4\u30EB\
  \u3092\u8AAD\u3080\u5FC5\u8981\u304C\u51FA\u305F\u306E\u306F\u30B3\u30F3\u30D4\u30E5\
  \u30FC\u30BF\u304C\u8A95\u751F\u3057\u305F\u5F53\u521D\u3067\u3059\u3002\u30C7\u30FC\
  \u30BF\u3092\u6C38\u7D9A\u7684\u306B\u4FDD\u5B58\u3059\u308B\u306B\u306F\u5916\u90E8\
  \u8A18\u61B6\u88C5\u7F6E\u306B\u4FDD\u5B58\u3057\u3001\u5F8C\u3067\u8AAD\u307F\u51FA\
  \u3059\u5FC5\u8981\u304C\u3042\u308A\u307E\u3057\u305F\u3002Swift\u3067\u306F\u3001\
  `String`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.513149-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6700\u521D\u306B\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\
  \u3080\u5FC5\u8981\u304C\u51FA\u305F\u306E\u306F\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\
  \u304C\u8A95\u751F\u3057\u305F\u5F53\u521D\u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\
  \u6C38\u7D9A\u7684\u306B\u4FDD\u5B58\u3059\u308B\u306B\u306F\u5916\u90E8\u8A18\u61B6\
  \u88C5\u7F6E\u306B\u4FDD\u5B58\u3057\u3001\u5F8C\u3067\u8AAD\u307F\u51FA\u3059\u5FC5\
  \u8981\u304C\u3042\u308A\u307E\u3057\u305F\u3002Swift\u3067\u306F\u3001`String`\
  \ \u30AF\u30E9\u30B9\u3092\u4F7F\u3063\u3066\u30D5\u30A1\u30A4\u30EB\u306E\u5185\
  \u5BB9\u3092\u7C21\u5358\u306B\u8AAD\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002\u3057\u304B\u3057\u3001\u5927\u304D\u306A\u30D5\u30A1\u30A4\u30EB\u3084\u30D0\
  \u30A4\u30CA\u30EA\u30C7\u30FC\u30BF\u306E\u5834\u5408\u306F `NSData` \u3084 `InputStream`\
  \ \u3092\u4F7F\u3046\u3053\u3068\u304C\u4E00\u822C\u7684\u3067\u3059\u3002\u307E\
  \u305F\u3001\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u969B\u306B\u306F\u975E\u540C\
  \u671F\u51E6\u7406\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u30E6\u30FC\
  \u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u304C\u30D5\u30EA\u30FC\
  \u30BA\u3057\u306A\u3044\u3088\u3046\u306B\u3059\u308B\u3053\u3068\u304C\u5927\u5207\
  \u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to: (やり方)
```Swift
import Foundation

// テキストファイルのパスを指定
if let path = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        // ファイル内容を読み込んでStringに変換
        let content = try String(contentsOfFile: path, encoding: .utf8)
        print(content)
    } catch {
        // エラーが発生した場合はここで処理
        print("ファイル読み込みエラー: \(error)")
    }
} else {
    print("ファイルが見つかりません。")
}
```
出力サンプル:
```
こんにちは、世界！
これはテキストファイルの内容です。
```

## Deep Dive (深く掘り下げる)
最初にファイルを読む必要が出たのはコンピュータが誕生した当初です。データを永続的に保存するには外部記憶装置に保存し、後で読み出す必要がありました。Swiftでは、`String` クラスを使ってファイルの内容を簡単に読むことができます。しかし、大きなファイルやバイナリデータの場合は `NSData` や `InputStream` を使うことが一般的です。また、ファイルを読む際には非同期処理を利用することで、ユーザーインターフェースがフリーズしないようにすることが大切です。

## See Also (関連情報)
- [Swift Standard Library - Strings and Characters](https://developer.apple.com/documentation/swift/string)
- [Apple Developer Documentation - Bundle](https://developer.apple.com/documentation/foundation/bundle)
