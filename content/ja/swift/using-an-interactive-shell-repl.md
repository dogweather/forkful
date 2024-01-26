---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:18:32.327858-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ?
対話型シェル、またはRead-Eval-Print Loop（REPL）を使用することで、対話的にコーディングできます。プログラマーはこれを使用して、迅速にSwiftのスニペットをテストしたり、デバッグしたり、言語を学んだりします。

## どうやって:
REPLを起動するには、ターミナルを開き、`swift`を実行します。直接コードを入力してEnterを押すと、実行されます。ひと味わいを見てみましょう：

```Swift
1> let greeting = "こんにちは、REPL!"
greeting: String = "こんにちは、REPL!"
2> print(greeting)
こんにちは、REPL!
```

`:quit` または `Control-D` で終了します。

## より深く
REPLのルーツは、60年代のLispインタープリタまで遡ります。SwiftのREPLは、LLVMという強力なコンパイラフレームワークの上に構築されており、基本的な解釈以上のものを提供します。それは、オートコンプリート、デバッグなど、より充実したツールです。REPLは学習やプロトタイピングには最適ですが、スタンドアロンの開発環境ではありません。一部の人々は、よりグラフィカルでファイルベースのアプローチを好むため、XcodeのPlaygroundsを使用することを好みますが、他の人々は従来のスクリプトの編集や実行に固執します。

内部では、SwiftのREPLはコードを動的にマシン言語にコンパイルして実行するため、比較的高速です。また、コンパイルされたSwiftモジュールや、Cライブラリーにもアクセスできるため、非常に強力です。ただし、REPLで完璧に動作するわけではないことに注意してください。特に、複雑なプロジェクトのセットアップやストーリーボードファイルが必要とされるSwiftの機能のいくつかは、ここでは機能しません。

## 関連情報
- [Swift.org - 入門](https://www.swift.org/getting-started/#using-the-repl)
- Appleの[Xcode Playgroundsの紹介](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVMプロジェクト](https://llvm.org/)