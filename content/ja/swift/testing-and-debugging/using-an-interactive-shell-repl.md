---
date: 2024-01-26 04:18:32.327858-07:00
description: "\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\u3001\u307E\u305F\u306FRead-Eval-Print\
  \ Loop\uFF08REPL\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u5BFE\
  \u8A71\u7684\u306B\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u3067\u304D\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u7528\u3057\u3066\
  \u3001\u8FC5\u901F\u306BSwift\u306E\u30B9\u30CB\u30DA\u30C3\u30C8\u3092\u30C6\u30B9\
  \u30C8\u3057\u305F\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3057\u305F\u308A\u3001\u8A00\
  \u8A9E\u3092\u5B66\u3093\u3060\u308A\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.615879-06:00'
model: gpt-4-0125-preview
summary: "\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\u3001\u307E\u305F\u306FRead-Eval-Print\
  \ Loop\uFF08REPL\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u5BFE\
  \u8A71\u7684\u306B\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u3067\u304D\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u4F7F\u7528\u3057\u3066\
  \u3001\u8FC5\u901F\u306BSwift\u306E\u30B9\u30CB\u30DA\u30C3\u30C8\u3092\u30C6\u30B9\
  \u30C8\u3057\u305F\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3057\u305F\u308A\u3001\u8A00\
  \u8A9E\u3092\u5B66\u3093\u3060\u308A\u3057\u307E\u3059\u3002."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
