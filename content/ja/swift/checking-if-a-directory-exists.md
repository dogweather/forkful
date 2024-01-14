---
title:    "Swift: ディレクトリの存在を確認する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

**なぜディレクトリが存在するかチェックするのか？**

ディレクトリが存在するかどうかを確認することは、コンピューターやネットワーク上のファイルを管理する際に非常に重要な役割を果たします。例えば、ファイルを読み込む前に、そのファイルが存在するかどうかをチェックすることで、未定義の動作を防ぐことができます。このように、ディレクトリの存在チェックはプログラミングにおいてとても便利な方法です。

**ディレクトリが存在するかどうかをチェックする方法**

まず、Swiftで提供されている`FileManager`クラスを使用して、ファイルマネージャーのインスタンスを作成します。次に、`fileExists(atPath:)`メソッドを使用して、チェックしたいディレクトリのパスを指定します。最後に、戻り値を`true`または`false`の真偽値で受け取ることができます。

```
Swift
let fileManager = FileManager()
let directoryPath = "/Users/username/Documents/"
let isDirectoryExists = fileManager.fileExists(atPath: directoryPath)

print(isDirectoryExists) // Output: true or false
```

ここで、チェックしたいディレクトリのパスを指定する際に注意しなければならないのが、絶対パスを指定することです。` /Users/username/Documents/`のように、コンピューター上の完全なパスを指定する必要があります。

**ディープダイブ**

ディレクトリが存在するかどうかをチェックする方法は非常に簡単ですが、その背後にはより深い仕組みがあります。実際には、ファイルマネージャーが指定されたパスのファイルやフォルダーが存在するかどうかを確認し、その結果を`true`または`false`で返すという仕組みになっています。

しかし、ファイルマネージャーのメソッドは基本的にはエラーを処理するためのものであり、`fileExists(atPath:)`メソッドも例外ではありません。そのため、もしファイルが存在しない場合にエラーが起こる可能性もあるため、コードを実行する際にはエラー処理をしっかりと行うことが重要です。

**参考リンク**

- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Standard Library - FileManager](https://docs.swift.org/swift-book/LanguageGuide/AccessControl.html)
- [Swift Doc - FileManager.fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1416773-fileexists)
- [Swift Doc - Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)

## はてな

- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Standard Library - FileManager](https://docs.swift.org/swift-book/LanguageGuide/AccessControl.html)
- [Swift Doc - FileManager.fileExists(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1416773-fileexists)
- [Swift Doc - Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)