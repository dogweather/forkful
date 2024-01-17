---
title:                "一時ファイルの作成 -"
html_title:           "Swift: 一時ファイルの作成 -"
simple_title:         "一時ファイルの作成 -"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ?
一時的なファイルを作成するとは、プログラマーが使用する仮のファイルのことであり、通常は一時的なデータの保管に使用されます。このようなファイルは、プログラム実行中に必要な一時的な情報を保持するために使用され、実行が完了した後に削除されます。

## 方法:
Swiftでは、`NSTemporaryDirectory()`メソッドを使用して一時的なファイルを作成できます。以下のコードを使用して、一時的なファイルを作成し、削除する方法を確認してみましょう。

```Swift
// 一時的なファイルを作成する
let tempDir = NSTemporaryDirectory()
let tempFileURL = URL(fileURLWithPath: "\(tempDir)/example.txt")
FileManager.default.createFile(atPath: tempFileURL.path, contents: nil, attributes: nil)

// ファイルが作成されたことを確認する
print(FileManager.default.fileExists(atPath: tempFileURL.path))

// ファイルを削除する
do {
    try FileManager.default.removeItem(at: tempFileURL)
} catch let error {
    print("Error: \(error.localizedDescription)")
}

// ファイルが削除されたことを確認する
print(FileManager.default.fileExists(atPath: tempFileURL.path))
```

実行結果:

```
true
false
```

## 深く掘り下げる:
一時的なファイルの概念は、旧来のコンピューターシステムで使用されていたメモリ管理の手法の一部として起源を持っています。他の代替案としては、メモリ内の一時的な領域を使用する方法や、プログラムが終了すると自動的に削除される一時的なファイルを作成する方法があります。Swiftでは、`UUID()`を使用して一意のファイル名を生成し、重複を防ぐようにすることが推奨されています。

## 関連情報:
- [NSTemporaryDirectory() - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/1409219-nstemporarydirectory)
- [Managing Temporary Files in Swift - AppCoda](https://www.appcoda.com/swift-temporary-files/)