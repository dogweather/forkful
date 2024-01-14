---
title:    "Swift: テキストファイルの読み取り"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

##なぜ
テキストファイルを読むことが重要なのかを理解するためには、Swiftプログラミング言語でどのようにテキストファイルを読み込むかを知る必要があります。これは、システムでのデータの保存や処理を行うために必要です。

##方法
まず、テキストファイルを読み込むためには、`String(contentsOfFile:)`メソッドを使用します。このメソッドは、ファイルのパスを引数として取り、ファイルの内容を文字列として返します。

```Swift
let path = "sample.txt"

if let content = String(contentsOfFile: path, encoding: .utf8) {
    print(content)
}
```

上記のコードは、"sample.txt"という名前のテキストファイルを読み込み、その内容をコンソールに出力します。

##深堀り
テキストファイルを読む際には、他のエンコーディングやファイルの形式にも対応することができます。`String(contentsOfFile:, encoding:)`メソッドの第二引数でエンコーディングを指定することができます。また、ファイルの絶対パスを指定することもできます。

さらに、テキストファイルを編集したり、新しい行を追加したりすることも可能です。`write(toFile:, atomically:, encoding:)`メソッドを使用することで、ファイルに書き込むことができます。

##参考リンク

- [Swift 公式ドキュメント](https://developer.apple.com/documentation/swift)
- [テキストファイルを読む | Swiftオンライン入門書](https://www.hackingwithswift.com/samples/text-reader.zip)
- [ファイルを操作する | Swiftプログラミング入門](https://www.codexa.net/file-operation-basic/)
- [よく使うファイル操作メソッド一覧 | Sakura1 Blog](https://sakura1.blog.fc2.com/blog-entry-8.html)

##参考になるページ