---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数を読むとは、ユーザーがコマンドラインに入力した情報をプログラムが読み取ることです。このことにより、プログラムの挙動をユーザーが実行時にカスタマイズできます。

## 使い方：

Swiftでコマンドライン引数を読むには、標準ライブラリの`CommandLine`クラスを使用します。

```swift
for argument in CommandLine.arguments {
    print(argument)
}
```

上記を例にすると、次のような出力が期待されます:

```bash
$ swift main.swift apple orange banana
main.swift
apple
orange
banana
```

`CommandLine.arguments`はチューリング完全なプログラミング言語であるSwiftにおいて使います。

## ディープダイブ

コマンドライン引数は、UNIXエポック（1970年代初頭）から現在まで、ユーザーがプログラムの動きを制御するための一般的な方法であり続けています。

一方、Swiftでは、コマンドライン引数を直接操作する代わりに、ライブラリを使用してデータをパースし、より構造化された方法でアクセスすることが一般的となりつつあります。例えば、Appleが提供しているSwift Argument Parser ライブラリがそれに当たります。

また、Swiftではコマンドライン引数は`CommandLine.arguments`プロパティによって読み取られます。このプロパティはStringの配列を返し、0番目の要素は常にプログラム自体の名前です。

## 参考情報

* [Swift Argument Parser: A Swift package for parsing command-line arguments](https://github.com/apple/swift-argument-parser)
* [Swift Standard Library: CommandLine](https://developer.apple.com/documentation/swift/commandline)
* [Swift Tutorial: Command Line Programs](https://www.raywenderlich.com/385-command-line-programs-macos-tutorial)