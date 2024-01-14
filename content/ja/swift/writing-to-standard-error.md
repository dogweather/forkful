---
title:                "Swift: 「標準エラーへの書き込み」"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

エラーメッセージを標準エラー出力に書き込むことの意義を説明します。

エラーメッセージを標準エラー出力に書き込むことは、デバッグや問題の特定に役立ちます。また、実行中のプログラムでエラーが発生した場合に、ユーザーにエラーの原因を伝えることができます。

## 使い方

以下は、Swiftで標準エラー出力にエラーメッセージを書き込む方法の例です。

```
Swift guard let file = FileManager.default.contents(atPath: "notExistingFile") else {
    fatalError("ファイルが見つかりませんでした")
}

// program continues...
```

上記のコードでは、`FileManager`を使用して指定したファイルを読み込み、その結果を`guard`文でオプショナル型の変数`file`に代入しています。もしファイルが見つからない場合は、`fatalError`関数を使用してエラーメッセージを標準エラー出力に書き込みます。

実行すると、以下のような結果になります。

```
Fatal error: ファイルが見つかりませんでした
```

## 深堀り

標準エラー出力に書き込まれるエラーメッセージは黄色で表示され、標準出力に書き込まれたものよりも目立ちます。これは、標準エラー出力がエラーメッセージのみを表示するように設計されているためです。

また、標準エラー出力に書き込まれるエラーメッセージは、ファイルやコマンドラインでプログラムを実行した際に、ファイルにリダイレクトすることなく直接ターミナル上に表示することができます。

## 参考

- [Swift Language Guide - Standard Library](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID340)
- [Apple Developer Documentation - Logging and Diagnostic Messages](https://developer.apple.com/documentation/xcode/logging_and_diagnostic_messages)