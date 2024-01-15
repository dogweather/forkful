---
title:                "一時ファイルの作成"
html_title:           "Swift: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は、プログラムの実行中に一時的にデータを保存する必要がある場合です。例えば、ユーザーがアプリで添付ファイルを一時的に編集する場合や、アプリがデータベースから情報を取得して一時的に保存する場合などがあります。

## 作り方

一時ファイルを作成する際は、まず `FileManager` クラスを使用して一時フォルダを作成します。その後、 `URL` クラスを使用して作成したフォルダ内に一時ファイルを作成し、データを書き込みます。

```Swift
let fileManager = FileManager.default
do {
    // Temporary folder is created
    let temporaryFolder = try fileManager.url(
        for: .itemReplacementDirectory,
        in: .userDomainMask,
        appropriateFor: URL(fileURLWithPath: "/"),
        create: true
    )
    // Temporary file is created inside the temporary folder
    let temporaryFile = temporaryFolder.appendingPathComponent("sample.txt")
    // Data is written to the temporary file
    try "This is a temporary file.".write(to: temporaryFile, atomically: true, encoding: .utf8)
} catch {
    print(error)
}
```

上記のコードを実行すると、一時ファイルが作成され、指定した内容が書き込まれることが確認できます。

## 詳細を調べる

一時ファイルを作成する際、`FileManager` クラスの `url(for:in:appropriateFor:create:)` メソッドはファイルシステムに依存して動作します。また、一時フォルダの作成に使用する `FileManager` オブジェクトは、他のオブジェクトよりも一時フォルダの優先順位が低いことに注意が必要です。

## 関連リンク

- [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [URL - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/url)
- [Understanding temporary file handling in Swift - Hacking with Swift](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-in-swift)