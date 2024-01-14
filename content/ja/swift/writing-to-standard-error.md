---
title:    "Swift: 「標準エラーに書き込む」"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

なぜ「標準エラー」に書き込む必要があるのでしょうか？Swiftプログラミングで、標準エラーを利用するメリットについてご紹介します。

## 方法

標準エラーへの書き込み方法はとても簡単です。下記のコード例をご覧ください。

```Swift
var standardError = FileHandle.standardError
let data = "エラーが発生しました".data(using: .utf8)
standardError?.write(data!)
```

実行すると、次のように標準エラーにメッセージが表示されます。

```
エラーが発生しました
```

## 深堀り

標準エラーへの書き込みは主にエラーハンドリング時に使用されます。例えば、ファイルの読み込みに失敗した場合や、不正な入力があった場合などに、エラーメッセージを標準エラーに表示することができます。また、標準エラーは標準出力とは別のストリームを持つため、バグのデバッグやログを出力する際にも便利に使えます。

## あわせて読みたい

- [Swiftのエラーハンドリングについてのドキュメント](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [FileHandleクラスについてのドキュメント](https://developer.apple.com/documentation/foundation/filehandle)