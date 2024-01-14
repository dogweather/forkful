---
title:    "Swift: 一時ファイルの作成"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

あなたはSwiftプログラミングをするとき、一時ファイルの作成をする理由があるかもしれません。時には、アプリケーションが作業中に一時ファイルを使用する必要があるかもしれません。

## 方法

```Swift
let tempDir = NSTemporaryDirectory()
let tempFileURL = URL(fileURLWithPath: tempDir).appendingPathComponent("temporaryfile.txt")

do {
    try "This is a temporary file!".write(to: tempFileURL, atomically: true, encoding: .utf8)
    let content = try String(contentsOf: tempFileURL)
    print(content)
} catch {
    print("Error creating temporary file: \(error)")
}
```

このコードでは、まず一時ディレクトリを取得し、そのパスに任意のファイル名を付けてファイルURLを作成します。次に、ファイルを作成し、データを書き込みます。最後に、ファイルを読み込んで内容を確認します。

## 深堀

一時ファイルは、アプリケーションの実行中に一時的に作成され、後で削除されるファイルです。これは、アプリケーションが作業中に必要なデータを保存するために使用されることがあります。また、一時ファイルを使用することで、アプリケーションのパフォーマンスが向上することがあります。

## 詳細

一時ファイルは、通常、アプリケーションが終了した後や、不要になった後に自動的に削除されます。また、一時ファイルの作成や削除には、ファイルシステムへのアクセスが必要なため、パフォーマンスに影響を与える可能性があります。そのため、多くの場合、一時ファイルはメモリ上で処理されることが推奨されます。ただし、データが大きい場合や、アプリケーションの実行中に一時ファイルを更新する必要がある場合は、ファイルシステム上に一時ファイルを作成することもできます。

## 参考リンク

- [Apple Developer Documentation: NSTemporaryDirectory()](https://developer.apple.com/documentation/foundation/nstemporarydirectory)
- [Apple Developer Documentation: URL(fileURLWithPath:)](https://developer.apple.com/documentation/foundation/url/1407962-fileurlwithpath)
- [Apple Developer Documentation: String(contentsOf:)](https://developer.apple.com/documentation/foundation/string/2994873-init)
- [Swift Knowledge Base: How do I create a temporary file?](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-on-disk-using-nsfilemanager)