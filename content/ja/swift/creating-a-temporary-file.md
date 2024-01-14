---
title:                "Swift: 一時ファイルを作成する"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# なぜ
一時ファイルを作成する理由は、プログラムが実行中に一時的にデータを保持する必要があるからです。例えば、メモリを消費しないように、プログラムの一時的な計算結果を保存したり、データを一時的に保管したりする場合に使用されます。

## 作り方
一時ファイルを作成するには、以下のようなコードを使用します。

```Swift
let fileManager = FileManager.default  // ファイルマネージャーを初期化
let temporaryDirectory = NSTemporaryDirectory()  // 一時フォルダのパスを取得
let filename = "temporaryFile.txt"  // 一時ファイルの名前を指定
let temporaryFileURL = NSURL.fileURL(withPathComponents: [temporaryDirectory, filename]) // 一時ファイルのパスを作成
fileManager.createFile(atPath: temporaryFileURL.path, contents: nil, attributes: nil) // ファイルマネージャーを使って一時ファイルを作成

// 一時ファイルにデータを書き込む
let dataString = "This is a temporary file"
let myStringData = dataString.data(using: String.Encoding.utf8)!
try? myStringData.write(to: temporaryFileURL)

// 一時ファイルからデータを読み込む
let data = try? Data(contentsOf: temporaryFileURL)
let stringData = NSString(data: data!, encoding: String.Encoding.utf8.rawValue)
print(stringData) // "This is a temporary file"

// 一時ファイルを削除
try? fileManager.removeItem(at: temporaryFileURL)
```

## 深堀り
一時ファイルを作成する方法は色々ありますが、上記の方法が最も一般的です。一時ファイルを作成する際には、重複したファイル名を避けるためにランダムな名前を生成することも重要です。また、ファイルを作成し、データを書き込んだ後に、ファイルを削除することも忘れないようにしましょう。

## 関連リンク
- [Apple Developer Documentation: FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Technical Note TN2406: Temporary File and Directory Management in macOS](https://developer.apple.com/library/archive/technotes/tn2406/_index.html)