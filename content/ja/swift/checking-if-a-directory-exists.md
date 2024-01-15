---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Swift: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ？

ディレクトリには、ファイルやデータを効率的に整理するための重要な役割があります。そのため、プログラマーはしばしばコード内でディレクトリをチェックする必要があります。

## 手順

ディレクトリが存在するかどうかを確認するには、`FileManager`クラスを使用します。以下の例では、`fileExists(atPath:)`メソッドを使用して、ディレクトリが存在するかどうかをチェックしています。

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/username/Documents"
if fileManager.fileExists(atPath: directoryPath) {
    print("Directory exists!")
} else {
    print("Directory does not exist.")
}
```

上記のコードを実行すると、指定したディレクトリが存在する場合には「Directory exists!」というメッセージが、存在しない場合には「Directory does not exist.」というメッセージが出力されます。

## さらに深く

ディレクトリが存在するかどうかをチェックする場合、`fileExists(atPath:)`メソッドは絶対パスを引数として受け取ります。しかし、相対パスを使用したい場合には、`fileExists(atPath:, isDirectory:)`メソッドを使用することもできます。このメソッドでは、第二引数に`isDirectory`を`true`に設定することで、ファイルではなくディレクトリをチェックすることができます。

## See Also

- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift by Sundell - Checking if a file or directory exists in Swift](https://www.swiftbysundell.com/articles/checking-if-a-file-or-directory-exists-in-swift)