---
title:    "Swift: 一時ファイルを作成する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作成することによって、Swiftプログラムで一時的にデータを保存することができます。

## 方法
一時ファイルを作成する方法は簡単です。まず、`NSTemporaryDirectory()`関数を使用して一時ディレクトリのパスを取得します。次に、そのパスを使用して`FileManager`クラスをインスタンス化し、`createFile(atPath:contents:attributes:)`メソッドを使用してファイルを作成します。最後に、一時ファイルを使用した処理が完了したら、`removeItem(atPath:)`メソッドを使用してファイルを削除します。

```Swift
let tempDir = NSTemporaryDirectory()
let fileManager = FileManager.default

// Create temporary file
let tempFilePath = tempDir.appendingPathComponent("tempFile.txt")
let fileContent = "This is temporary file content"
let fileData = fileContent.data(using: .utf8)
fileManager.createFile(atPath: tempFilePath, contents: fileData, attributes: nil)

// Read temporary file
let tempFileURL = URL(fileURLWithPath: tempFilePath)
let tempFileData = try? Data(contentsOf: tempFileURL)
if let tempFileData = tempFileData, let tempFileContent = String(data: tempFileData, encoding: .utf8) {
    print(tempFileContent) // Output: "This is temporary file content"
}

// Delete temporary file
try? fileManager.removeItem(atPath: tempFilePath)
```

## ディープダイブ
一時ファイルを作成する方法についてさらに詳しく見ていきましょう。一時ディレクトリは、アプリケーションが割り当てられたストレージ領域の一部であり、アプリケーションが実行中に生成されたファイルを保管する場所です。アプリケーションが終了すると、一時ディレクトリはクリーンアップされ、一時ファイルも自動的に削除されます。

また、一時ファイルを作成する際には、作成されたファイルのパスを保持しておくことをお勧めします。これにより、ファイルの使用が終了した後に削除することができます。

## 参考リンク
- [Apple Developer Documentation: NSTemporaryDirectory()](https://developer.apple.com/documentation/foundation/1409851-nstemporarydirectory)
- [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [raywenderlich: iOS File Management Tutorial: Working with Directories](https://www.raywenderlich.com/5574-ios-file-management-tutorial-working-with-directories)