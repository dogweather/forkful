---
title:                "標準エラーへの書き込み"
html_title:           "Swift: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーに書き込むことは、バグやエラーのデバッグに非常に有用です。プログラムを実行した際に、標準出力では見つけられないエラーを標準エラーでキャッチすることができます。

## 方法

まず、`print()` 関数を使用して標準出力にテキストを出力します。しかし、標準エラーにテキストを出力する場合は、`print()` 関数ではなく`FileHandle.standardError`を使用します。下記のコードを試してみてください。

```
Swift guard let fileHandle = FileHandle.standardError else { exit(EXIT_FAILURE) }

let errorMessage = "エラーが発生しました。"

fileHandle.write(errorMessage.data(using: .utf8)!)

```

上記のコードを実行すると、`エラーが発生しました。`というメッセージが標準エラーに出力されます。

## ディープダイブ

Swiftでは、バグやエラーを検出するために使用するさまざまな方法があります。標準エラーへの書き込みはその1つです。しかし、標準エラーに出力されるメッセージを読み取ることは、プログラマーにとっては簡単ではありません。そのため、標準エラーに出力されるエラーメッセージをより詳細にカスタマイズすることができるように、`FileManager`オブジェクトの`FileHandle`を使用することができます。また、`FileManager`を使用して標準エラーをファイルにリダイレクトすることもできます。

## さらに参考になるリンク

- [Swift公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [標準エラーと標準出力の違いについて](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.3.0/com.ibm.zos.v2r3.bpxbd00/rub.html)
- [標準出力と標準エラーの読み取り方法](https://qiita.com/hosiawase_tyan/items/70f8b6b857b784decacd)