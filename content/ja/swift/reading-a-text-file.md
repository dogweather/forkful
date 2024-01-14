---
title:    "Swift: 「テキストファイルの読み込み」"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ
あなたがテキストファイルを読むことに興味を持っているかもしれません。それは、データを解析したり、アプリケーションで使用したりするためです。この記事では、Swiftプログラミング言語でテキストファイルを読む方法を紹介します。

## 方法
まず、テキストファイルを読み込むためにファイルマネージャーを作成します。次に、ファイルマネージャーの `contentsOfFile` メソッドを使用してファイルを読み込み、ファイルの内容を `NSString` オブジェクトまたは配列に保存します。例えば、以下のようになります。

```Swift 
if let textPath = Bundle.main.path(forResource: "sample", ofType: "txt") {
    if let textContents = try? String(contentsOfFile: textPath) {
        let stringArray = textContents.components(separatedBy: "\n")
        print(stringArray)
    }
}
```

このコードの最初の行では、ファイルのパスを取得しています。次に、`contentsOfFile` メソッドを使用してファイルの内容を `NSString` オブジェクトに保存し、`components` メソッドを使用して改行文字で文字列を分割し、配列に保存しています。最後に、配列を出力してファイルの内容を確認できます。

## ディープダイブ
この記事では、基本的なテキストファイルの読み込み方法を紹介しましたが、テキストファイルの種類やデータの形式によっては、さまざまな方法があります。また、ファイルの読み込みだけでなく、書き込みや編集も可能です。これらの方法を詳しく学び、より高度なテキストファイルの処理を行うことができるようになると、より多くの用途に応用できるプログラムを作成できるでしょう。

## 参考 
- [Apple公式ドキュメント](https://developer.apple.com/documentation/foundation/filemanager)
- [テキストファイルの読み込みと書き込み](https://www.ralfebert.de/snippets/ios/swift-3-textfile/#reading-and-writing-text-files)
- [テキストファイルの改行コードの扱い方](https://stackoverflow.com/questions/33327378/read-a-text-file-line-by-line-in-swift-2)

## 関連記事 
[テキストファイルを読み込む際の注意点](https://www.techdoze.net/basic-reading-text-files-in-swift/)