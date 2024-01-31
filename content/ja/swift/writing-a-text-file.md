---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？
テキストファイルを書くとは、データをテキスト形式でファイルに保存することです。プログラマーは設定、ログ、またはユーザーデータを保存するためにこれを行います。

## How to:
## 方法:
```Swift
import Foundation

let contents = "こんにちは、Swift!"
let filePath = "/path/to/your/file.txt"

do {
    try contents.write(toFile: filePath, atomically: true, encoding: .utf8)
    print("ファイルの保存に成功しました。")
} catch {
    print("エラー: \(error)")
}
```

## Deep Dive
## 掘り下げ
ファイルの書き込みはUNIX時代からあります。Swiftでは`FileHandle`や`FileManager`での書き込みも可能ですが、簡単なテキストデータは`String`の`write(toFile:atomically:encoding:)`メソッドが便利です。これにより、文字エンコーディングの問題も簡単に解決できます。

## See Also
## 関連リンク
- [Swift Standard Library](https://developer.apple.com/documentation/swift/swift_standard_library)
- [Working with Files in Swift on iOS](https://developer.apple.com/documentation/foundation/filemanager)
