---
date: 2024-01-26 04:18:32.327858-07:00
description: "\u3069\u3046\u3084\u3063\u3066: REPL\u3092\u8D77\u52D5\u3059\u308B\u306B\
  \u306F\u3001\u30BF\u30FC\u30DF\u30CA\u30EB\u3092\u958B\u304D\u3001`swift`\u3092\u5B9F\
  \u884C\u3057\u307E\u3059\u3002\u76F4\u63A5\u30B3\u30FC\u30C9\u3092\u5165\u529B\u3057\
  \u3066Enter\u3092\u62BC\u3059\u3068\u3001\u5B9F\u884C\u3055\u308C\u307E\u3059\u3002\
  \u3072\u3068\u5473\u308F\u3044\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\uFF1A\
  ."
lastmod: '2024-03-13T22:44:42.615879-06:00'
model: gpt-4-0125-preview
summary: "REPL\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\u3001\u30BF\u30FC\u30DF\u30CA\
  \u30EB\u3092\u958B\u304D\u3001`swift`\u3092\u5B9F\u884C\u3057\u307E\u3059\u3002\u76F4\
  \u63A5\u30B3\u30FC\u30C9\u3092\u5165\u529B\u3057\u3066Enter\u3092\u62BC\u3059\u3068\
  \u3001\u5B9F\u884C\u3055\u308C\u307E\u3059\u3002\u3072\u3068\u5473\u308F\u3044\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
