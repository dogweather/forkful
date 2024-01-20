---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:58:45.630944-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリ存在確認は、指定されたパスにディレクトリが存在するかどうかをチェックすることです。プログラマーは、ファイル操作を行う前にエラーを避けるためにこれを行います。

## How to: (やり方)
Swiftでは、`FileManager`クラスを使用してディレクトリの存在を確認します。こんな感じです：

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("ディレクトリが存在します。")
} else {
    print("ディレクトリが存在しません。")
}
```

サンプル出力：

```
ディレクトリが存在します。
```
または

```
ディレクトリが存在しません。
```

## Deep Dive (深堀り)
`fileExists(atPath:)`メソッドは、NeXTSTEP時代からあります。ディレクトリのチェックには他にも方法があります。例えば、`attributesOfItemAtPath(_:error:)`を試してファイル属性を取得したり、ファイルシステムイベントを監視するなどです。しかし、簡潔さと直接性から、`fileExists(atPath:)`がよく使用されます。`isDirectory`ポインタを利用すると、パスがディレクトリかどうかも判別できます。

## See Also (関連情報)
- [Apple Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Documentation on fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1410277-fileexists)
- [Stack Overflow: How to check if a directory exists in Objective-C](https://stackoverflow.com/questions/5667799/how-to-check-if-a-directory-exists-in-objective-c) (Objective-C版の情報も役に立ちます)