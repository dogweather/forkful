---
date: 2024-01-20 17:53:37.027120-07:00
description: "How to: (\u3084\u308A\u65B9) Swift\u3067\u306F\u3001`print`\u95A2\u6570\
  \u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u304C\
  \u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u57FA\u672C\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.617127-06:00'
model: gpt-4-1106-preview
summary: "Swift\u3067\u306F\u3001`print`\u95A2\u6570\u3092\u4F7F\u3063\u3066\u7C21\
  \u5358\u306B\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u304C\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306F\u57FA\u672C\u4F8B\u3067\u3059."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

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
