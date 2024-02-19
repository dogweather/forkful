---
aliases:
- /ja/swift/printing-debug-output/
date: 2024-01-20 17:53:37.027120-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u3069\u3046\u52D5\u3044\u3066\u3044\
  \u308B\u304B\u3092\u898B\u308B\u305F\u3081\u306B\u3001\u30C7\u30D0\u30C3\u30B0\u51FA\
  \u529B\uFF08\u30C7\u30D0\u30C3\u30B0\u7528\u306E\u60C5\u5831\u8868\u793A\uFF09\u3092\
  \u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30D0\u30B0\u306E\
  \u539F\u56E0\u3092\u7279\u5B9A\u3057\u305F\u308A\u3001\u5909\u6570\u306E\u72B6\u614B\
  \u3092\u78BA\u8A8D\u3057\u305F\u308A\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.231772
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u3069\u3046\u52D5\u3044\u3066\u3044\
  \u308B\u304B\u3092\u898B\u308B\u305F\u3081\u306B\u3001\u30C7\u30D0\u30C3\u30B0\u51FA\
  \u529B\uFF08\u30C7\u30D0\u30C3\u30B0\u7528\u306E\u60C5\u5831\u8868\u793A\uFF09\u3092\
  \u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30D0\u30B0\u306E\
  \u539F\u56E0\u3092\u7279\u5B9A\u3057\u305F\u308A\u3001\u5909\u6570\u306E\u72B6\u614B\
  \u3092\u78BA\u8A8D\u3057\u305F\u308A\u3067\u304D\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムがどう動いているかを見るために、デバッグ出力（デバッグ用の情報表示）を行います。これにより、バグの原因を特定したり、変数の状態を確認したりできます。

## How to: (やり方)
Swiftでは、`print`関数を使って簡単にデバッグ出力ができます。以下は基本例です。

```swift
let message = "こんにちは、Swift！"
print(message)
```

このコードの出力は:

```
こんにちは、Swift！
```

オブジェクトや複数の変数も一緒に出力できます。

```swift
let name = "たろう"
let age = 25
print("名前: \(name), 年齢: \(age)")
```

このコードの出力は:

```
名前: たろう, 年齢: 25
```

## Deep Dive (深掘り)
デバッグ出力はプログラミングの初期から利用されています。Swiftの`print`以外にも、`NSLog`や標準エラー出力に`stderr`を使う方法があります。

`NSLog`は時間やアプリケーション名を含む情報を付加します。ただし、パフォーマンスが少し落ちる可能性があります。

```swift
NSLog("何かログに残すメッセージ")
```

標準エラーに出力するには、`FileHandle`を利用します。

```swift
import Foundation

let error = "エラーが発生しました。"
if let data = "\(error)\n".data(using: .utf8) {
    FileHandle.standardError.write(data)
}
```

各メソッドは状況に応じて使い分けましょう。`print`はデバッグ時、`NSLog`は本番環境のログ、`FileHandle`はエラーメッセージに適しています。

## See Also (関連情報)
- Swiftの公式ドキュメント: [https://docs.swift.org/swift-book/](https://docs.swift.org/swift-book/)
- Appleのログ記録ガイド: [https://developer.apple.com/documentation/os/logging](https://developer.apple.com/documentation/os/logging)
- Stack Overflowでのディスカッション: [https://stackoverflow.com/questions/tagged/swift](https://stackoverflow.com/questions/tagged/swift)
