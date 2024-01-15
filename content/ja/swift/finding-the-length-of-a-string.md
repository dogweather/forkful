---
title:                "文字列の長さを見つける"
html_title:           "Swift: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ文字列の長さを求める必要があるのか

文字列の長さを知ることは、プログラミングにおいて非常に重要です。例えば、ユーザーが入力したデータを処理する際に、その入力が指定した文字数以内であるかを確認したり、特定の文字数の範囲に収まるようにフォーマットする際に必要となります。

## 方法

文字列の長さを求める方法は簡単です。まず、`count`プロパティを使用します。これは、Swiftの`String`クラスに含まれており、文字列の長さを返します。

```Swift
let string = "Hello World"
print(string.count) // 出力結果：11
```

また、Stringの`characters`プロパティを使用して、このように長さを求めることもできます。

```Swift
let string = "こんにちは"
print(string.characters.count) // 出力結果：5
```

## 深く掘り下げる

文字列の長さを求めるという単純なタスクにもかかわらず、実はややややこしい部分があります。Unicodeという仕組みにより、文字列の「長さ」は特定の言語に依存する場合があるため、注意が必要です。例えば、`"こんにちは"`という文字列の長さは5である一方、`"你好"`という文字列の長さは2になります。これは、日本語と中国語が異なる文字コードを使用しているためです。

## See Also

- [String - The Swift Programming Language (Swift 4.1)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID268)
- [Unicode - Wikipedia](https://ja.wikipedia.org/wiki/Unicode)