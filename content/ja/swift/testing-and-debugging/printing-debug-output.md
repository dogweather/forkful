---
title:                "デバッグ出力を表示する"
aliases: - /ja/swift/printing-debug-output.md
date:                  2024-01-20T17:53:37.027120-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/printing-debug-output.md"
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
