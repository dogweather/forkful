---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Go: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認する事は、指定したパスにディレクトリが実際に存在するかを検証する処理です。これを行う理由は、ディレクトリが存在しない場合にプログラムが異常終了するのを防ぐためや、的確なファイル操作を行うためです。

## 実行方法:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/directory"

if fileManager.fileExists(atPath: directoryPath) {
    print("\(directoryPath) exists")
} else {
    print("\(directoryPath) does not exist")
}
```

これを実行すると、指定したディレクトリが存在するかどうかに応じたメッセージが表示されます。

## ディープダイブ:

- **歴史的背景**: Swiftの初期バージョンから、ディレクトリの存在を確認するためのAPIが提供されています。これは、OSのファイルシステムを抽象化し、安全かつ効率的なファイル操作を可能にするためです。
- **代替案**: `fileExists(atPath:)`は、存在を確認するパスがファイルかディレクトリかを区別しないため、ディレクトリ専用であることを確認したい場合は、`attributesOfItem(atPath:)`メソッドを使い、`FileAttributeType`が`.typeDirectory`となるか確認します。
- **実装詳細**: `fileExists(atPath:)`メソッドは内部的にstatシステムコールを使用して、指定されたパスの情報を取得し、その情報からディレクトリの存在を確認します。

## 参考資料:

- [Apple Developer Documentation: fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1410277-fileexists)
- [Apple Developer Documentation: attributesOfItem(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1412643-attributesofitem)
- [Stack Overflow: How to check if a directory exists in Swift?](https://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file)