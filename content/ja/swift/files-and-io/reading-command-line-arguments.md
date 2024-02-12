---
title:                "コマンドライン引数の読み取り"
aliases:
- /ja/swift/reading-command-line-arguments.md
date:                  2024-01-20T17:56:58.745705-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むのは、プログラム実行時にユーザーが提供する情報を受け取ることです。これにより、動的に振る舞いを変更でき、柔軟なツールやスクリプトが作成可能になります。

## How to: (方法)
```Swift
// main.swift
for arg in CommandLine.arguments {
    print(arg)
}

// 実行例: $ swift run MyProgram foo bar
// 出力例:
// /path/to/MyProgram
// foo
// bar
```
このコードは、プログラムに渡されたすべての引数を出力します。`CommandLine.arguments`を使ってアクセスします。

## Deep Dive (深い潜水)
コマンドライン引数の利用は歴史的に古く、C言語の`int main(int argc, char *argv[])`から始まりました。Swiftでは、`CommandLine`クラスがこれを簡単に提供します。文字列の配列でアクセスでき、最初の要素はプログラムのパスです。アプリケーションやツールを作る際には、フラグパーサーを使うのが一般的です（例: `Swift Argument Parser`ライブラリ）。これにより、エラーハンドリングも容易になり、ユーザーフレンドリーなインターフェースを提供できます。

## See Also (関連情報)
- Swift Argument Parser documentation: [https://github.com/apple/swift-argument-parser](https://github.com/apple/swift-argument-parser)
- Swift Documentation for CommandLine: [https://developer.apple.com/documentation/swift/commandline](https://developer.apple.com/documentation/swift/commandline)
- Building command-line tools with Swift: [https://www.swiftbysundell.com/articles/building-a-command-line-tool-using-the-swift-package-manager/](https://www.swiftbysundell.com/articles/building-a-command-line-tool-using-the-swift-package-manager/)
