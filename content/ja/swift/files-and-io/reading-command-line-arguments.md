---
date: 2024-01-20 17:56:58.745705-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u306E\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\u30E6\u30FC\
  \u30B6\u30FC\u304C\u63D0\u4F9B\u3059\u308B\u60C5\u5831\u3092\u53D7\u3051\u53D6\u308B\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\u7684\u306B\
  \u632F\u308B\u821E\u3044\u3092\u5909\u66F4\u3067\u304D\u3001\u67D4\u8EDF\u306A\u30C4\
  \u30FC\u30EB\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u304C\u4F5C\u6210\u53EF\u80FD\u306B\
  \u306A\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:16.182484-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u306E\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\u30E6\u30FC\
  \u30B6\u30FC\u304C\u63D0\u4F9B\u3059\u308B\u60C5\u5831\u3092\u53D7\u3051\u53D6\u308B\
  \u3053\u3068\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u52D5\u7684\u306B\
  \u632F\u308B\u821E\u3044\u3092\u5909\u66F4\u3067\u304D\u3001\u67D4\u8EDF\u306A\u30C4\
  \u30FC\u30EB\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u304C\u4F5C\u6210\u53EF\u80FD\u306B\
  \u306A\u308A\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
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
