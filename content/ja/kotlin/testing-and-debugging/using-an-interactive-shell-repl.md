---
date: 2024-01-26 04:15:59.244820-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Kotlin\u306EREPL\u3092\u8D77\u52D5\u3059\u308B\
  \u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u30BF\u30FC\u30DF\u30CA\u30EB\u3092\u958B\
  \u3044\u3066`kotlinc`\u3068\u5165\u529B\u3057\u307E\u3059\u3002\u3059\u308B\u3068\
  \u3001Kotlin\u30B7\u30A7\u30EB\u306B\u5165\u308A\u307E\u3059\u3002\u5909\u6570\u3092\
  \u5B9A\u7FA9\u3057\u3066\u305D\u306E\u5024\u3092\u51FA\u529B\u3057\u3066\u307F\u307E\
  \u3057\u3087\u3046\uFF1A."
lastmod: '2024-04-05T21:53:42.950089-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 使い方：
KotlinのREPLを起動するのは簡単です。ターミナルを開いて`kotlinc`と入力します。すると、Kotlinシェルに入ります。変数を定義してその値を出力してみましょう：

```kotlin
Kotlin version 1.7.10へようこそ (JRE 1.8.0_292-b10)
ヘルプには :help、終了するには:quitと入力してください
>>> val greeting = "Hello, Kotlin REPL!"
>>> println(greeting)
Hello, Kotlin REPL!
```

## 深掘り
KotlinのREPLは、言語とともに実験を奨励するために登場しました。それはPythonのインタラクティブシェルと似ていますが、Kotlinの構文や特性に合わせてカスタマイズされています。他の選択肢？ IntelliJ IDEAなどのIDE内のインタラクティブ環境、オンラインKotlinプレイグラウンドなどがあります。REPLは、コードをその場でコンパイルして、学習やデバッグに不可欠な即時フィードバックを提供します。

## 参照
- REPLについてのKotlinドキュメント：[https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- ブラウザでKotlinを試す：[https://play.kotlinlang.org](https://play.kotlinlang.org)
- IntelliJ IDEA用JetBrains Kotlin Playgroundプラグイン。
