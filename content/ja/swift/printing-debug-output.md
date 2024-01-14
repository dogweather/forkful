---
title:                "Swift: デバッグの出力を印刷する"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ
プログラムを書く際に、デバッグ用の出力を表示することが重要です。これにより、コードが期待どおりに動作しているかどうかを確認することができます。

## 方法
デバッグ用の出力を表示するには、Swiftの ```print()``` 関数を使用します。以下の例を参考にしてください。

```Swift
let number = 5
print("変数numberの値は\(number)です。")

// 出力結果：変数numberの値は5です。
```

このように、```print()```関数を使うと、任意のデータを出力することができます。また、複数の値を出力する際には、コンマで区切ることもできます。

```Swift
let name = "田中"
let age = 25
print("私の名前は\(name)です。年齢は\(age)才です。")

// 出力結果：私の名前は田中です。年齢は25才です。
```

## 深堀り
デバッグ用の出力を表示する際に便利なのが、オプションの引数です。例えば、出力するときの区切りや改行を指定することができます。

```Swift
print("Apple", "Banana", "Melon", separator: ", ", terminator: ".")
// 出力結果：Apple, Banana, Melon.
```

また、デバッグ用の出力を表示しながら、実際にアプリが実行される時には出力されないようにすることもできます。

```Swift
// デバッグ用の出力を表示
print("この部分のみデバッグモードでのみ出力されます。")

// アプリ実行時には出力されない
assertionFailure("このコードは実行されません。")
```

## 関連記事
- [Swift公式ドキュメント - print(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541053-print)
- [デバッグの基本 - Swiftスタートアップガイド](https://aktsk.hatenablog.com/entry/2014/06/29/215246)
- [Swiftでassertを使う方法 - Qiita](https://qiita.com/SatoshiKawabata/items/811b2ecf79529ed24662)