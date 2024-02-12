---
title:                "ディレクトリが存在するかどうかの確認"
aliases:
- /ja/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:09:00.419100-07:00
model:                 gpt-4-0125-preview
simple_title:         "ディレクトリが存在するかどうかの確認"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Swiftアプリケーション内からファイル構造を管理するため、ファイルシステム内にディレクトリが存在するかどうかを確認することは不可欠です。このタスクにより、開発者は読み取りまたは書き込みを試みる前にディレクトリの存在を確認でき、実行時エラーを回避することができます。

## 方法:

SwiftのFoundationフレームワークには、ファイルシステムを管理するための`FileManager`クラスが提供されています。`FileManager`を使用してディレクトリが存在するかどうかを確認できます。以下がその方法についてのスニペットです：

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

しかし、これはファイルとディレクトリの両方をチェックします。ディレクトリが確かに存在するかを確認したい場合は、`isDirectory`にブール値へのポインターを渡す必要があります：

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Directory exists")
} else {
    print("Directory does not exist")
}
```

### サードパーティライブラリを使用する

現在、Swiftにおいてディレクトリの存在を確認する場合、`FileManager`クラスの堅牢性のため、通常はサードパーティのライブラリを必要としません。しかし、より複雑なファイル操作と確認を行う場合、John Sundellが作成した**Files**のようなライブラリは、よりSwiftにフレンドリーなAPIを提供します。

以下はその使用方法です：

まず、Swift Package Managerを介してプロジェクトにFilesを追加します。

その後、以下のようにディレクトリの存在を確認できます：

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Directory exists")
} catch {
    print("Directory does not exist")
}
```

注：サードパーティのライブラリは変更される可能性があるため、常に最新のドキュメントを参照して、使用方法とベストプラクティスを確認してください。
