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

## 説明 & 必要性?
ディレクトリが存在するかどうかを確認することは、プログラマーにとって重要です。ディレクトリとは、ファイルやサブディレクトリを含む場所のことです。プログラマーは、特定のディレクトリが存在するかどうかを確認し、必要に応じて処理することができます。

## 方法:
```Swift
let fileManager = FileManager.default //デフォルトのファイルマネージャーを作成
let directoryExists = fileManager.fileExists(atPath: "/Users/username/Documents") //指定したパスのディレクトリが存在するかを確認
print(directoryExists) //出力: true or false
```

## 深く掘り下げる:
ディレクトリが存在するかどうかを確認する方法は、現在のバージョンのSwiftでは非常に簡単ですが、以前のバージョンでは少し異なっていました。以前のバージョンでは、ファイルマネージャーの `fileExistsAtPath` メソッドを使用する必要がありましたが、現在のバージョンでは `fileExists(atPath:)` メソッドが使用可能です。また、ディレクトリが存在しない場合に新しく作成する方法もあります。さらに、ディレクトリが存在しない場合にエラーを投げる方法や、特定の種類のファイルのみを含むディレクトリを確認する方法など、様々なオプションがあります。

## 参考:
- [Apple - FileManager Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Hacking with Swift - Checking if a file exists using FileManager](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-exists-using-filemanager)
- [Swift by Sundell - Working with directories and files in Swift](https://www.swiftbysundell.com/basics/directories-and-files/)