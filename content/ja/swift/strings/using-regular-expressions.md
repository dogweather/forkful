---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:44.847199-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u306E\u6B63\u898F\u8868\u73FE\u306E\u30CD\u30A4\
  \u30C6\u30A3\u30D6\u30B5\u30DD\u30FC\u30C8\u306F\u3001`NSRegularExpression`\u30AF\
  \u30E9\u30B9\u3068String\u30AF\u30E9\u30B9\u306E\u7BC4\u56F2\u304A\u3088\u3073\u7F6E\
  \u63DB\u30E1\u30BD\u30C3\u30C9\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30D6\u30ED\u30C3\u30AF\u5185\u306E\u30E1\u30FC\
  \u30EB\u30A2\u30C9\u30EC\u30B9\u3092\u898B\u3064\u3051\u3066\u30CF\u30A4\u30E9\u30A4\
  \u30C8\u3059\u308B\u305F\u3081\u306B\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\
  \u308B\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.598366-06:00'
model: gpt-4-0125-preview
summary: "Swift\u306E\u6B63\u898F\u8868\u73FE\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u30B5\
  \u30DD\u30FC\u30C8\u306F\u3001`NSRegularExpression`\u30AF\u30E9\u30B9\u3068String\u30AF\
  \u30E9\u30B9\u306E\u7BC4\u56F2\u304A\u3088\u3073\u7F6E\u63DB\u30E1\u30BD\u30C3\u30C9\
  \u3092\u5229\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u30C6\u30AD\u30B9\
  \u30C8\u30D6\u30ED\u30C3\u30AF\u5185\u306E\u30E1\u30FC\u30EB\u30A2\u30C9\u30EC\u30B9\
  \u3092\u898B\u3064\u3051\u3066\u30CF\u30A4\u30E9\u30A4\u30C8\u3059\u308B\u305F\u3081\
  \u306B\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B\u4F8B\u3067\u3059\uFF1A\
  ."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 方法：
Swiftの正規表現のネイティブサポートは、`NSRegularExpression`クラスとStringクラスの範囲および置換メソッドを利用します。以下は、テキストブロック内のメールアドレスを見つけてハイライトするために正規表現を使用する例です：

```swift
import Foundation

let text = "詳しくは、support@example.com または feedback@example.org までお問い合わせください。"
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("見つかりました: \(text[range])")
        }
    } else {
        print("一致するものが見つかりませんでした。")
    }
} catch {
    print("Regexエラー: \(error.localizedDescription)")
}

// サンプル出力：
// 見つかりました: support@example.com
// 見つかりました: feedback@example.org
```

より複雑または便利さを重視するシナリオについては、SwiftRegexのようなサードパーティライブラリを使用できます。これにより、構文が単純化され、可能性が広がります。Swiftの標準ライブラリは強力ですが、一部の開発者は、簡潔な構文と追加機能のためにこれらのライブラリを好む場合があります。以下は、仮想のサードパーティライブラリを使用して類似のタスクを実行する方法です：

```swift
// SwiftRegexというライブラリが存在し、インポートされていると仮定
let text = "hello@world.com に連絡するか、当社のウェブサイトにアクセスしてください。"
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // SwiftRegexによって提供される仮定のメソッド
if emails.isEmpty {
    print("メールアドレスが見つかりませんでした。")
} else {
    emails.forEach { email in
        print("見つかりました: \(email)")
    }
}

// `matches(for:)`メソッドがSwiftRegexに存在すると仮定した場合の想定出力：
// 見つかりました: hello@world.com
```

この例は、`matches(for:)`のような便利メソッドが存在すると仮定して、文字列内の一致を簡単に見つけるためにサードパーティ正規表現パッケージを使用する方法を説明しています。正確な構文とメソッドの可用性については、それぞれのサードパーティライブラリのドキュメントを参照してください。
