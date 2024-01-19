---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Swift"
category:             "Swift"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Swiftプログラミング: 新プロジェクトの開始 

## 何 & なぜ？
新しいプロジェクトを開始するとは、新たなプログラムを作り始めることです。これを通じて、プログラマーは創造性を活用し、必要なソフトウェアを作成することが可能になります。

## 方法:
新しいSwiftプロジェクトを始める基本手順を以下に示します。コードブロック```Swift..```内にサンプルコードと出力例が含まれています:

```Swift
// 新しいプロジェクトを作成するコマンド:
// Terminalで以下を入力
$ mkdir ProjectName
$ cd ProjectName
$ swift package init

// プロジェクトディレクトリへの移動後、
// main.swiftファイルに以下のコードを書きます:
import Swift
print("Welcome to the new Swift Project!")

// ターミナルで以下を実行してプロジェクトをビルドし、
// プログラムを実行します:
$ swift run
```

このコードを実行すると、"Welcome to the new Swift Project!"というメッセージが表示されます。

## ディープダイブ:
Swift言語はAppleが2014年に初めて発表され、iOS, macOS, watchOS, tvOSアプリケーションの開発に広く使用されています。上記で示したプロジェクトの開始方法は、完全な開発環境やエディタなどを使用せずにSwiftプロジェクトを始める最もシンプルな方法の一つです。

しかし、代替手段としてXcodeなどのIDEを使用することも可能です。XcodeはApple製アプリケーションの開発のための強力なツールで、新しいプロジェクトの設定、テスト、デバッグ、デプロイなどを支援します。

## 参考リンク:
以下に、Swift言語と新しいプロジェクトの開始に関するいくつかの参考リンクを提供します:
1. [Swift.org](https://swift.org/): 正式なSwiftプログラミング言語ウェブサイト
2. [Apple's Swift Programming Language Guide](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/index.html): Appleからの公式Swiftプログラミングガイド
3. [Getting started with Swift on Xcode](https://developer.apple.com/swift/): Xcodeを使用したSwiftの開始ガイド