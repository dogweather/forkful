---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時ファイルは、テンポラリなデータを保存するために作成される。プログラマは、大量のデータを処理する際や、データを転送中にバックアップとしてこれを使用します。

## 方法:

以下に、Swiftで一時ファイルを作成する方法を示します。

```Swift
import Foundation

let temporaryDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let temporaryFilename = ProcessInfo().globallyUniqueString
let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(temporaryFilename)

do {
    try "Hello, Swift!".write(to: temporaryFileURL, atomically: false, encoding: .utf8)
} catch {
    print("エラー: \(error)")
}
```

このコードでは、一時ディレクトリに一意な名前のテキストファイルが作成されます。そして、そのテキストファイルに"Hello, Swift!"という文字列が書き込まれます。エラー発生時には、エラー内容が表示されます。

## ディープダイブ:

一時ファイルの概念は、早い段階からコンピューターシステムに取り入れられました。これは、大量のデータを一時的に格納し、共有するための便利な方法と見なされました。

iOSやmacOSでは、一時ディレクトリが提供され、アプリケーションは一時ファイルをこの場所に保存することが一般的です。この節のコードスニペットでは、`NSTemporaryDirectory()` 関数を使用して一時ディレクトリのパスを取得していますが、これは非推奨とされています。代わりに推奨される方法として、ファイルマネージャーの `URLs(for:in:)` メソッドを使用して一時ディレクトリのURLを取得することが挙げられます。

また、作成するファイルの名前については、各ファイルが一意であることを保証するために、プロセス情報の `globallyUniqueString` プロパティを用いています。

## 参照:

- Apple開発者ドキュメンテーション: [URLs(for:in:)](https://developer.apple.com/documentation/foundation/filemanager/1407726-urls)
- Stack Overflow: ["How to create a temporary file with Swift?"](https://stackoverflow.com/questions/37696762/how-to-create-a-temporary-file-with-swift)