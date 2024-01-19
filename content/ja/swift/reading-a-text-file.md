---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストファイルの読み取りは、データへのアクセスやテキストファイル内の情報の利用を可能にします。プログラマはこれを用いて様々な用途（設定情報の読み込み、大量のテキストデータの解析など）に役立てています。

## 使い方：

テキストファイルの読み取りの基本的な方法をSwiftで見てみましょう。

```Swift
import Foundation

if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = dir.appendingPathComponent("file.txt")
   
    do {
        let text = try String(contentsOf: fileURL, encoding: .utf8)
        print(text)
    }
    catch {
        print("Failed reading from URL: \(fileURL), Error: " + error.localizedDescription)
    }
}
```

このコードは `file.txt`というテキストファイルの内容を表示します。存在しない場合や読み取りエラーが発生した場合は、エラーメッセージを表示します。

## ディープダイブ：
 
ファイルを読み取ることは、コンピュータが存在する以来の基本的な操作です。Swiftでは、標準ライブラリであるFoundationを使用してこれを実現しています。

代替手段としては、 `NSData` または `InputStream` を使用する方法があります。しかしこれらは多くの場合、 `String` の 'contentsOf' メソッドよりも複雑であることが多いです。

実装の詳細については、Swiftは内部でCのライブラリを利用してI/O操作を実行しています。これにより、高いパフォーマンスと同時に多機能性が実現されています。

## 関連リンク：
以下のリンクは、関連する情報をさらに探求したい方に役立つでしょう。

- [Apple's Swift Documentation](https://developer.apple.com/documentation/swift)
- [SwiftのStringドキュメンテーション](https://developer.apple.com/documentation/swift/string/)
- [File I/O in Swift](https://www.raywenderlich.com/666-file-manager-tutorial-for-ios-how-to-create-directories-read-files-and-more)