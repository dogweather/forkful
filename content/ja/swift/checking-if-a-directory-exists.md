---
title:                "Swift: ディレクトリが存在するかどうかを確認する"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
今日は、あなたにディレクトリが存在するかどうかをチェックする方法についてお話しします。これは、特定のファイルにアクセスする必要があるときや、プログラムが正常に動作しているかどうかを確認するために必要になります。

## 方法
まず、Swiftのファイルマネージャーを使用して、指定したディレクトリが存在するかどうかを確認することができます。次のコードを使用して、指定したパスにディレクトリが存在するかどうかをチェックすることができます。

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/Desktop/ExampleDirectory"

if fileManager.fileExists(atPath: directoryPath) {
    print("指定したディレクトリは存在します。")
} else {
    print("指定したディレクトリは存在しません。")
}
```

出力は、「指定したディレクトリは存在します。」か「指定したディレクトリは存在しません。」のいずれかになります。

また、アプリケーションのドキュメントフォルダー内の特定のディレクトリが存在するかどうかを確認したい場合は、次のコードを使用することができます。

```Swift
let directoryPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/ExampleDirectory"

if fileManager.fileExists(atPath: directoryPath) {
    print("指定したディレクトリは存在します。")
} else {
    print("指定したディレクトリは存在しません。")
}
```

出力も同様に「指定したディレクトリは存在します。」か「指定したディレクトリは存在しません。」のいずれかになります。

## 深堀り
ディレクトリが存在するかどうかをチェックするには、ファイルマネージャーの `fileExists(atPath:)` メソッドを使用しています。このメソッドは、指定したパスにファイルまたはディレクトリが存在するかどうかをブール値で返します。存在する場合は `true` を、存在しない場合は `false` を返します。

さらに、`fileExists(atPath:)` メソッドを使用することで、指定したディレクトリ内に存在するファイルやサブディレクトリの確認も行うことができます。また、ファイルを操作する前に、事前に存在を確認することでエラーを防ぐことができます。

## 参考リンク
- [Swiftで指定したディレクトリが存在するかどうかをチェックする方法 | Qiita](https://qiita.com/kitoko552/items/6993c1353efaf1e4cd2d)
- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Developer Documentation - NSSearchPathForDirectoriesInDomains](https://developer.apple.com/documentation/foundation/nssearchpathfordirectoriesindomains)

## もっと見る
- [Swiftでファイル操作を行う方法 | Qiita](https://qiita.com/shunp/items/0bc169ba900a705b4ad6)
- [Swiftにおけるアプリケーションのファイルシステムへのアクセス | Apple Developer Documentation](https://developer.apple.com/documentation/bundleresources/accessing_a_file_system_in_macos)
- [マークダウン記法の基本 | Qiita](https://qiita.com/tbpgr/items/989c6bade