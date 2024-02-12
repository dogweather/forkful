---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- /ja/kotlin/using-an-interactive-shell-repl/
date:                  2024-01-26T04:15:59.244820-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-an-interactive-shell-repl.md"
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
