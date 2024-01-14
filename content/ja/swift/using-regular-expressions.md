---
title:                "Swift: Regular Expressionsを使用する"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ 
 Regular Expressionsを使用するか

正規表現は、文字列の処理や検索を行う際に非常に便利なツールです。Swiftを使用して開発を行う際に、文字列を扱う場面は非常に多いため、正規表現を学ぶことで開発効率を上げることができます。

## 初め方

正規表現を使用するには、まずは正規表現のパターンを定義する必要があります。このパターンを使用して、文字列を検索や置換することができます。下記のように、```Swift```ブロック内にパターンを定義し、文字列を操作してみましょう。

```Swift
let pattern = "\\d+"
let string = "abc123"

if let range = string.range(of: pattern, options: .regularExpression) {
    let match = string.substring(with: range)
    print(match)
}

// Output: 123
```

文字列の中から、数字のみを抽出することができました。これは非常に便利ですね！正規表現には様々なパターンがあり、様々な文字列を検索や置換することができます。自分の開発に合わせたパターンを学ぶことで、より効率的に文字列を操作することができます。

## 深く学ぶ

正規表現に関する詳細な情報は、公式ドキュメントや書籍にて学ぶことができます。また、オンライン上には多くのサンプルコードやコミュニティが存在しており、他の開発者がどのように正規表現を使用しているかを学ぶこともできます。正規表現は一度学ぶとその知識は幅広い開発に応用することができるため、しっかりと学ぶことが重要です。

## 参考リンク

- [Swift正規表現チュートリアル](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [正規表現入門](https://www.amazon.co.jp/dp/4798060651)
- [正規表現パターン](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID335) (公式ドキュメント) 

## 参考にする

- [Swiftで文字列操作をする際の Tips](https://qiita.com/YutaSaito1991/items/61933c87a431c2b02da7)
- [正規表現を使った Swift の文字列操作方法](https://tmasuda37.net/archives/1213)