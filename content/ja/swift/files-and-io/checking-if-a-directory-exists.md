---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.419100-07:00
description: "Swift\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u304B\u3089\
  \u30D5\u30A1\u30A4\u30EB\u69CB\u9020\u3092\u7BA1\u7406\u3059\u308B\u305F\u3081\u3001\
  \u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u5185\u306B\u30C7\u30A3\u30EC\u30AF\
  \u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\
  \u3059\u308B\u3053\u3068\u306F\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u3053\u306E\u30BF\
  \u30B9\u30AF\u306B\u3088\u308A\u3001\u958B\u767A\u8005\u306F\u8AAD\u307F\u53D6\u308A\
  \u307E\u305F\u306F\u66F8\u304D\u8FBC\u307F\u3092\u8A66\u307F\u308B\u524D\u306B\u30C7\
  \u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u78BA\u8A8D\u3067\u304D\u3001\
  \u5B9F\u884C\u6642\u30A8\u30E9\u30FC\u3092\u56DE\u907F\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.747389
model: gpt-4-0125-preview
summary: "Swift\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u304B\u3089\u30D5\
  \u30A1\u30A4\u30EB\u69CB\u9020\u3092\u7BA1\u7406\u3059\u308B\u305F\u3081\u3001\u30D5\
  \u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u5185\u306B\u30C7\u30A3\u30EC\u30AF\u30C8\
  \u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\
  \u308B\u3053\u3068\u306F\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u3053\u306E\u30BF\u30B9\
  \u30AF\u306B\u3088\u308A\u3001\u958B\u767A\u8005\u306F\u8AAD\u307F\u53D6\u308A\u307E\
  \u305F\u306F\u66F8\u304D\u8FBC\u307F\u3092\u8A66\u307F\u308B\u524D\u306B\u30C7\u30A3\
  \u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u78BA\u8A8D\u3067\u304D\u3001\u5B9F\
  \u884C\u6642\u30A8\u30E9\u30FC\u3092\u56DE\u907F\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
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
