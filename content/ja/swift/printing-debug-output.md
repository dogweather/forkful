---
title:    "Swift: デバッグ出力の印刷"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力を印刷することの利点を説明します。デバッグ出力を印刷することによって、コードの実行中に何が起こっているのかを確認することができ、問題の特定や修正に役立ちます。

## 作り方

デバッグ出力を印刷する方法を簡単なコーディング例と共に説明します。まず、`print()`関数を使用して文字列を印刷し、その後に任意の変数や定数を追加して出力の種類を変えることができます。以下のコードを試してみてください。

```Swift
// 文字列の印刷
let message = "こんにちは！"
print(message)

// 変数の印刷
let name = "太郎"
print("おはよう！私の名前は\(name)です。")

// 複数の変数を含む出力
let score = 90
let subject = "数学"
print("\(subject)の成績は\(score)点です。")
```

上のコードを実行すると、以下のような出力が得られます。

```
こんにちは！
おはよう！私の名前は太郎です。
数学の成績は90点です。
```

## 深堀り

デバッグ出力を印刷する際には、以下の点に注意してください。

- `print()`関数はコンソールやデバッガーに出力しますが、ビルドされたアプリには影響しません。
- 一時的なデバッグ用のコードであるため、本番環境でコードを残さないようにしましょう。
- `print()`関数は複雑なオブジェクトを印刷する際には、内部で`description`プロパティを使用しているため、カスタムオブジェクトを正しく印刷することができない場合があります。そのため、カスタムオブジェクトのプロパティを印刷する方法を別途調べる必要があります。

## 参考

[Apple公式ドキュメント - デバッグの読み取りや出力を行う](https://developer.apple.com/jp/documentation/DebuggingTips.pdf)

[Ray Wenderlich - How to Debug in Swift: Advanced Debugging and Custom Printing](https://www.raywenderlich.com/10463346-how-to-debug-in-swift-advanced-debugging-and-custom-printing)

[Nomikomu Engineersブログ - デバッグプリントの書き方](https://blog.nomikomu-engineers.com/how-to-print-debug-output-in-swift-956dc63e15af)