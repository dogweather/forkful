---
title:                "Swift: 「文字列を大文字にする」"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

こんにちは、Swiftプログラミングの皆さん！今日は、文字列を大文字に変換する方法についてお話しします。文字列を大文字にすることは、データの整形や可読性の向上などさまざまな理由で行われます。では、さっそく始めましょう！

## なぜ文字列を大文字にするのか？

文字列を大文字にすることで、データの整形を行うことができます。例えば、ユーザーが名前を入力する際に、全ての文字が大文字で入力されるようにすることで、データの一貫性を保つことができます。また、大文字にすることで文字列の可読性も向上するため、プログラムがより分かりやすくなります。

## 方法

文字列を大文字にするには、Swiftの `String` メソッドの `.uppercased()` を使用します。このメソッドを使うことで、文字列を全て大文字に変換することができます。以下のコード例をご覧ください。

```Swift
let string = "hello world"
let capitalizedString = string.uppercased()

print(capitalizedString)
// 出力: HELLO WORLD
```

また、特定の部分だけ大文字にする場合は、 `.capitalized` メソッドを使うこともできます。以下のコード例をご覧ください。

```Swift
let greeting = "hello, my name is John"
let capitalizedGreeting = greeting.capitalized

print(capitalizedGreeting)
// 出力: Hello, My Name Is John
```

## もっと深く掘り下げる

ただ文字列を大文字にするだけではありません。実際には、使用しているメソッドやビルトイン関数、そしてその実装方法など、より詳細な情報が存在します。ご自身で調べてみることや、Swiftの公式ドキュメントを参照することで、さらに深く知ることができます。

## 関連リンク

- [Swift公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Stringメソッドリファレンス](https://developer.apple.com/documentation/swift/string)