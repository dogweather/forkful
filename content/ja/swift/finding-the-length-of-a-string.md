---
title:    "Swift: 文字列の長さを見つける"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングを学ぶ上で、文字列の長さを求めることは非常に重要です。SwiftのStringクラスには、文字列の長さを取得するための便利なメソッドが用意されています。この記事では、その方法を紹介します。

## 使い方

基本的にStringクラスの`count`プロパティを使用することで、文字列の長さを求めることができます。以下の例をご覧ください。

```Swift
let str = "こんにちは、世界！"
print(str.count) // 出力：9
```

また、絵文字や日本語など、文字列の長さを正しく計算することができない特殊な文字が入っている場合は、`NSString`クラスを使用することで正確な文字数を取得することもできます。詳しくは次のセクションで説明します。

## 深堀り

文字列の長さを正確に取得する方法は、文字コードによって異なります。一般的な文字には1つの文字コードが割り当てられていますが、特殊な文字の場合は複数の文字コードが使用されます。そのため、`count`プロパティで取得できる文字数は、単に文字数を数えただけではなく、文字コードも考慮されます。

しかし、日本語のように2バイトの文字を使用する言語では、`count`プロパティでは誤った文字数が返されてしまいます。そのため、正確な文字数を求めるためには`NSString`クラスの`length`メソッドを使用することが推奨されています。以下の例をご覧ください。

```Swift
let str = "こんにちは、世界！"
print(str.count) // 出力：9
print((str as NSString).length) // 出力：12
```

このように、文字列の長さを求める際には、 `count`プロパティと`length`メソッドのどちらを使うか、文字コードによって異なることに注意しながら判断する必要があります。

## 他にも見る

- [Swift公式ドキュメント - String](https://developer.apple.com/documentation/swift/string)
- [NSString Class Reference](https://developer.apple.com/documentation/foundation/nsstring)