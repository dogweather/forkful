---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.419100-07:00
description: "\u65B9\u6CD5: Swift\u306EFoundation\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\
  \u30AF\u306B\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u3092\u7BA1\
  \u7406\u3059\u308B\u305F\u3081\u306E`FileManager`\u30AF\u30E9\u30B9\u304C\u63D0\u4F9B\
  \u3055\u308C\u3066\u3044\u307E\u3059\u3002`FileManager`\u3092\u4F7F\u7528\u3057\u3066\
  \u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\u3046\
  \u304B\u3092\u78BA\u8A8D\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\
  \u65B9\u6CD5\u306B\u3064\u3044\u3066\u306E\u30B9\u30CB\u30DA\u30C3\u30C8\u3067\u3059\
  \uFF1A."
lastmod: '2024-03-13T22:44:42.637185-06:00'
model: gpt-4-0125-preview
summary: "Swift\u306EFoundation\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306B\u306F\
  \u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u3092\u7BA1\u7406\u3059\u308B\
  \u305F\u3081\u306E`FileManager`\u30AF\u30E9\u30B9\u304C\u63D0\u4F9B\u3055\u308C\u3066\
  \u3044\u307E\u3059\u3002`FileManager`\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\
  \u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\
  \u8A8D\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u65B9\u6CD5\u306B\
  \u3064\u3044\u3066\u306E\u30B9\u30CB\u30DA\u30C3\u30C8\u3067\u3059\uFF1A."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
