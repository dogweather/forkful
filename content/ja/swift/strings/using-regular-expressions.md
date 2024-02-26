---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:44.847199-07:00
description: "\u6B63\u898F\u8868\u73FE\u3001\u307E\u305F\u306Fregex\u306F\u3001\u691C\
  \u7D22\u30D1\u30BF\u30FC\u30F3\u3092\u5F62\u6210\u3059\u308B\u6587\u5B57\u306E\u30B7\
  \u30FC\u30B1\u30F3\u30B9\u3067\u3001\u6587\u5B57\u5217\u306E\u4E00\u81F4\u3084\u64CD\
  \u4F5C\u306E\u30BF\u30B9\u30AF\u306B\u3088\u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\
  \u3002\u30C7\u30FC\u30BF\u691C\u8A3C\u3084\u30D1\u30FC\u30B9\u304B\u3089\u5909\u63DB\
  \u307E\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u3089\u3092\
  \u5229\u7528\u3057\u3066\u3001Swift\u3092\u542B\u3080\u69D8\u3005\u306A\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u51E6\
  \u7406\u3084\u64CD\u4F5C\u30BF\u30B9\u30AF\u306B\u304A\u3044\u3066\u6B20\u304B\u305B\
  \u306A\u3044\u30C4\u30FC\u30EB\u306B\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.551270-07:00'
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\u3001\u307E\u305F\u306Fregex\u306F\u3001\u691C\u7D22\
  \u30D1\u30BF\u30FC\u30F3\u3092\u5F62\u6210\u3059\u308B\u6587\u5B57\u306E\u30B7\u30FC\
  \u30B1\u30F3\u30B9\u3067\u3001\u6587\u5B57\u5217\u306E\u4E00\u81F4\u3084\u64CD\u4F5C\
  \u306E\u30BF\u30B9\u30AF\u306B\u3088\u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\
  \u30C7\u30FC\u30BF\u691C\u8A3C\u3084\u30D1\u30FC\u30B9\u304B\u3089\u5909\u63DB\u307E\
  \u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u3089\u3092\u5229\
  \u7528\u3057\u3066\u3001Swift\u3092\u542B\u3080\u69D8\u3005\u306A\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\
  \u3084\u64CD\u4F5C\u30BF\u30B9\u30AF\u306B\u304A\u3044\u3066\u6B20\u304B\u305B\u306A\
  \u3044\u30C4\u30FC\u30EB\u306B\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
正規表現、またはregexは、検索パターンを形成する文字のシーケンスで、文字列の一致や操作のタスクによく使用されます。データ検証やパースから変換まで、プログラマーはそれらを利用して、Swiftを含む様々なプログラミング言語でのテキスト処理や操作タスクにおいて欠かせないツールにしています。

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
