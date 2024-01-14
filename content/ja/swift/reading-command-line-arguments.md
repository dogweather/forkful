---
title:                "Swift: コンピュータプログラミングにおける記事のタイトル：コマンドライン引数の読み込み"
simple_title:         "コンピュータプログラミングにおける記事のタイトル：コマンドライン引数の読み込み"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることの重要性は？

プログラムを実行する際に、ユーザーがコマンドライン引数を使用することで、プログラムにさまざまなパラメーターを渡すことができます。このように、プログラムの動作をユーザーが制御できることができるため、コマンドライン引数の読み取りは非常に重要です。

## 手順

コマンドライン引数を読み取るためのSwiftのコーディング例と、サンプルの出力を以下のコードブロックでご紹介します。

```Swift
// 引数が入力されているかどうかを確認
if CommandLine.arguments.count > 1 {
    // 最初の引数を取得
    let firstArgument = CommandLine.arguments[1]
    
    // 引数がInt型に変換可能かどうかをチェック
    if let number = Int(firstArgument) {
        // 引数が数字だった場合の処理
        print("数字が入力されました！\(number)")
    } else {
        // 引数が数字以外だった場合の処理
        print("数字以外が入力されました！")
    }
} else {
    // 引数が入力されていない場合の処理
    print("引数が入力されていません。")
}
```

サンプル入力と出力：

```
$ swift test.swift 10
数字が入力されました！10

$ swift test.swift hello
数字以外が入力されました！

$ swift test.swift
引数が入力されていません。
```

## 詳細

さらに深くコマンドライン引数を理解するために、以下の情報をご紹介します。

### コマンドライン引数とは

コマンドライン引数とは、プログラムを実行する際に、プログラムに渡すことができる文字列や数字などのパラメーターです。通常、プログラムを実行する際には、アプリケーション名の後にスペースを入れてパラメーターを入力します。

### コマンドライン引数の取得方法

Swiftでは、`CommandLine`クラスを使用してコマンドライン引数を取得することができます。このクラスには、プログラムの実行時に渡された引数の数や値を取得するメソッドや変数が用意されています。

## See Also

- [Swift 公式ドキュメント](https://developer.apple.com/documentation/swift/)
- [CommandLine - Swift Standard Library](https://developer.apple.com/documentation/foundation/commandline)
- [Swift コマンドライン引数の取得方法](https://code.i-harness.com/ja/q/286531)