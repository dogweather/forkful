---
date: 2024-01-26 04:15:59.244820-07:00
description: "REPL\uFF08Read-Eval-Print Loop\uFF09\u306F\u3001\u30B7\u30F3\u30D7\u30EB\
  \u306A\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u306A\u30B3\u30F3\u30D4\u30E5\
  \u30FC\u30BF\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u8FC5\u901F\u306A\u30B3\u30FC\u30C7\u30A3\
  \u30F3\u30B0\u8A66\u884C\u3001\u30B9\u30CB\u30DA\u30C3\u30C8\u306E\u30C6\u30B9\u30C8\
  \u3001\u307E\u305F\u306F\u5B8C\u5168\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u3092\u4F5C\u6210\u305B\u305A\u306B\u8A00\u8A9E\u306E\u69CB\u6587\u3092\u5B66\
  \u3076\u305F\u3081\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.220455
model: gpt-4-0125-preview
summary: "REPL\uFF08Read-Eval-Print Loop\uFF09\u306F\u3001\u30B7\u30F3\u30D7\u30EB\
  \u306A\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u306A\u30B3\u30F3\u30D4\u30E5\
  \u30FC\u30BF\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u8FC5\u901F\u306A\u30B3\u30FC\u30C7\u30A3\
  \u30F3\u30B0\u8A66\u884C\u3001\u30B9\u30CB\u30DA\u30C3\u30C8\u306E\u30C6\u30B9\u30C8\
  \u3001\u307E\u305F\u306F\u5B8C\u5168\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u3092\u4F5C\u6210\u305B\u305A\u306B\u8A00\u8A9E\u306E\u69CB\u6587\u3092\u5B66\
  \u3076\u305F\u3081\u306B\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
REPL（Read-Eval-Print Loop）は、シンプルなインタラクティブなコンピュータプログラミング環境です。プログラマは、迅速なコーディング試行、スニペットのテスト、または完全なアプリケーションを作成せずに言語の構文を学ぶために使用します。

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
