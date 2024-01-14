---
title:    "Swift: コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングを学ぶときにコマンドライン引数について聞いたことがあるかもしれません。それは、コマンドライン引数は、プログラムが実行されたときに渡される追加の情報であり、プログラムの機能を拡張するために役立ちます。Swiftでは、コマンドライン引数を簡単に読み込むことができます。この記事では、なぜコマンドライン引数を読み込むのか、そしてどのように読み込むのかについて説明します。

## 方法

コマンドライン引数を読み込むには、`CommandLine`クラスを使用します。例を見てみましょう。

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

上記のコードでは、`CommandLine`クラスの`arguments`変数を使用して、コマンドライン引数を読み込んでいます。実行すると、プログラムが実行された際に渡された引数がプリントされます。例えば、`swift MyProgram.swift arg1 arg2`というコマンドでプログラムを実行した場合、出力は`["MyProgram.swift", "arg1", "arg2"]`となります。

また、特定の引数を取得するには、`CommandLine`クラスの`arguments`変数を使用して、配列のインデックス番号を指定するだけです。例えば、`swift MyProgram.swift arg1 arg2`というコマンドでプログラムを実行した場合、`arguments[1]`は`"arg1"`を表します。

## 深堀り

コマンドライン引数を使うと、プログラムのオプションや設定を柔軟に変更することができます。例えば、ユーザーがプログラム実行時に引数を与えることで、プログラムの挙動や処理を変えることができます。

また、コマンドライン引数の前にハイフンをつけることで、さらに細かいオプションを設定することもできます。例えば、`swift MyProgram.swift -v -l 20`というコマンドでプログラムを実行した場合、`-v`オプションでバージョンを表示し、`-l`オプションで各行の最大文字数を設定することができます。このように、コマンドライン引数はプログラムの柔軟性を高めることができます。

## 詳しくは

Swiftでコマンドライン引数を扱う方法については、公式ドキュメントを参考にすることができます。
- [Command Line Arguments](https://developer.apple.com/documentation/swift/commandline)
- [The Swift Programming Language: Command Line Argument](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID163)

## 参考になるリンク

コマンドライン引数以外にも、Swiftの初心者に役立つ情報を紹介します。

- [はじめにSwift](https://www.gihyo.co.jp/book/2014/978-4-7741-6363-3)
- [Swift講座 - ドットインストール](https://dotinstall.com/lessons/basic_swift_v3)
- [Swiftコンパイラー - ApexClearCode](https://www.apexcancode.com/swift/)
- [Swiftプログラミング入門 - Udemy](https://www.udemy.com/course/swift-programming-for-beginners/)