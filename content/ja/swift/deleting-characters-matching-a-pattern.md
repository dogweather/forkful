---
title:                "Swift: パターンに一致する文字を削除する"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字をパターンに合致するものを削除することについて、どのように取り組むのかについて、これから説明します。

## どのように取り組むか

パターンに合致する文字を削除する方法には、いくつかの方法があります。例えば、正規表現を使用して削除することができます。

```Swift
let regex = try! NSRegularExpression(pattern: "[aeiou]", options: .caseInsensitive)
let string = "Hello Swift"
let modifiedString = regex.stringByReplacingMatches(in: string, options: [], range: NSMakeRange(0, string.utf16.count), withTemplate: "")
print(modifiedString) // Hll Swft
```

## 深堀

文字を削除する際、正規表現のパターンについてしっかりと理解することが重要です。また、パターンに合致した文字以外を削除する方法もありますので、それらを試してみることも大切です。

## 参考

- [NSRegularExpression Class Reference](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift Regular Expressions](https://nshipster.com/swift-regular-expressions/)
- [Playing with Swift Regular Expressions](https://medium.com/@abhimuralidharan/playing-with-swift-regular-expressions-9ec5bea3396a)

## 他に見る

- [文字列を操作する方法](https://techacademy.jp/magazine/24647)
- [今すぐ使えるSwiftの文字列操作テクニック](https://www.appbrewery.co/p/ios11-learn-how-to-build-chat-apps-using-facebook-messenger/)
- [Regex Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)