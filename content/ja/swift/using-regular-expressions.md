---
title:                "正規表現の使用"
aliases:
- ja/swift/using-regular-expressions.md
date:                  2024-02-03T19:18:44.847199-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
