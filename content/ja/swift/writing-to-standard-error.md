---
title:                "Swift: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

今回は、Swiftプログラミングにおいて「スタンダードエラー」を書き込む理由について説明していきます。

## Why

スタンダードエラーを書き込むことには、デバッグやエラー処理を行う際に役立ちます。エラーメッセージをスタンダードエラーに書き込むことで、プログラムの実行中に発生したエラーを確認することができます。

## How To

スタンダードエラーに書き込むには、```stderr```を使用します。以下のようにコードを記述することで、任意のメッセージをスタンダードエラーに書き込むことができます。

```Swift
import Darwin

let message = "エラーが発生しました。"

fputs(message, stderr)
```

実行すると、コンソールには何も表示されませんが、スタンダードエラーにはメッセージが書き込まれます。

## Deep Dive

スタンダードエラーに書き込むことで、エラー処理をより細かく行うことができます。例えば、特定のエラーが発生した時にはプログラムを停止させたい場合には、```exit(EXIT_FAILURE)```を使用することでプログラムを終了させることができます。

また、```stderr```はエラーメッセージ以外にも、警告やログなどのメッセージを書き込むのにも使用することができます。

See Also
- [Swift 公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html#ID494)
- [Stanford オンラインコース「Developing iOS 9 Apps with Swift」](https://itunes.apple.com/app/id1089643302?mt=8)