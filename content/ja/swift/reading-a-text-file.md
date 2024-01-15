---
title:                "テキストファイルの読み込み"
html_title:           "Swift: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読むのか

テキストファイルを読むことは、プログラミングにおいて重要なスキルです。テキストファイルからデータを取得することで、プログラムがより柔軟になり、さまざまなタスクを実行することができるようになります。

## どのようにする

テキストファイルを読むには、以下のようなSwiftのコードを使用します。

```Swift
// ファイルパスを指定してテキストファイルを開く
let fileURL = URL(fileURLWithPath: "sample.txt")

// ファイルを読み取り専用で開く
let file = try FileHandle(forReadingFrom: fileURL)

// ファイルからデータを読み込む
let data = file.readDataToEndOfFile()

// テキストファイルを閉じる
file.closeFile()

// テキストファイルの内容をString型に変換
let text = String(data: data, encoding: .utf8)

print(text)
```

上記のコードを実行すると、テキストファイルの内容がコンソールに出力されます。

## ディープダイブ

テキストファイルを読む際には、ファイルのエンコーディングや改行コードなどの細かい設定にも注意する必要があります。また、大きなテキストファイルを読み込む場合には、パフォーマンスの観点からバッファリングや非同期処理を考慮することも重要です。詳細については、Swiftの公式ドキュメントを参照してください。

## 関連リンク

- [Swift 公式ドキュメント](https://developer.apple.com/documentation/swift)
- [FileHandle クラスリファレンス](https://developer.apple.com/documentation/foundation/filehandle)
- [テキストファイルを読み書きする方法](https://www.sekky0905.com/2015/09/08/reading-and-writing-text-files-in-swift/)