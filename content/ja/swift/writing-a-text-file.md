---
title:    "Swift: 「テキストファイルの書き方」"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜテキストファイルを書くのか

プログラミングでテキストファイルを作成することには、さまざまな理由があります。たとえば、データを保存したり、ログを記録したり、テキストファイル形式でファイルをエクスポートしたりするためです。

## 方法

テキストファイルを書く方法を説明します。まず、新しいファイルを作成し、書き込むテキストを定義します。次に、`write(to: Atomically: Encoding:)`メソッドを使用して、テキストをファイルに書き込みます。最後に、ファイルを閉じて、保存することができます。

```Swift
let fileName = "myFile.txt"
let text = "これはテキストファイルに書き込まれた文章です。"
do {
    try text.write(to: URL(fileURLWithPath: fileName), atomically: true, encoding: .utf8)
    print("ファイルの書き込みに成功しました。")
} catch {
    print("ファイルの書き込みに失敗しました。")
}
```

実行結果：

```
ファイルの書き込みに成功しました。
```

## 深堀り

テキストファイルを書く際には、いくつかの重要なことがあります。たとえば、ファイルを閉じることが必要です。また、文字コードを選択することも重要です。さらに、オプションでファイルをアトミックに書き込むこともできます。これにより、ファイルの書き込み中にエラーが発生した場合に、ファイルを破損することなく元の状態に戻すことができます。

## さらに見る

- [Swiftでテキストファイルを書き込む方法](https://www.swift-studies.com/blog/swift-text-file-write/)
- [Swift公式ドキュメント: Writing Files](https://developer.apple.com/documentation/foundation/filemanager/1412653-write)
- [Swiftでテキストファイルから情報を読み込む方法](https://patientprogrammer.com/reading-text-file-in-swift/)