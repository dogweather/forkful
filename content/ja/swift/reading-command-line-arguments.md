---
title:                "コンピュータプログラミングにおけるコマンドライン引数の読み取り"
html_title:           "Swift: コンピュータプログラミングにおけるコマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングにおけるコマンドライン引数の読み取り"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何？なんで？
コマンドライン引数の読み込みとは、プログラマーがプログラムに与えられた入力を読み取ることです。プログラマーがコマンドライン引数を読み込むのは、ユーザーからの入力を受け取ったり、プログラムの動作をカスタマイズしたりするためです。

## 方法：
```Swift
let commandLineArgs = CommandLine.arguments
print(commandLineArgs)
```

上記のように、`CommandLine.arguments`を使用してコマンドライン引数を読み込むことができます。また、`print()`を使用することで、プログラムが受け取った引数を表示することができます。

## 詳細を掘り下げる：
コマンドライン引数の読み込みは、プログラミング言語によって異なる方法で行われます。Swiftでは、`CommandLine.arguments`を使用することで簡単に読み込むことができます。また、`arguments`にはプログラム名（通常は最初の要素）も含まれることに注意しましょう。コマンドライン引数の読み込みを行わずに、直接ユーザーからの入力を受け取ることも可能ですが、コマンドライン引数を使用することで、プログラムの実行時に引数を与えることができます。

## 関連リンク：
- [Swift公式ドキュメント（日本語）](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [コマンドライン引数の読み込み方法の記事（英語）](https://www.raywenderlich.com/896-swift-tutorial-part-2-a-simple-ios-app)
- [コマンドライン引数の活用方法の記事（英語）](https://www.ralfebert.de/snippets/ios/command-line/)