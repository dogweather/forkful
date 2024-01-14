---
title:                "Swift: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使う理由はたくさんあります。例えば、大量のテキストの中から特定のパターンを簡単に見つけたり、複雑な文字列を検索したりする時に役立ちます。Swiftでの正規表現の使用は、よりスマートかつ効率的なプログラミングを実現するための重要なツールです。

## 使い方

まずは、正規表現を使うために必要な`Foundation`フレームワークをインポートします。次に、`NSRegularExpression`のインスタンスを作成し、パターンとオプションを指定します。

```Swift
import Foundation

let text = "Swiftは素晴らしいプログラミング言語です。正規表現を使って文字列を検索したり、置換したりすることができます。"

let pattern = "正規表現"

let regex = try! NSRegularExpression(pattern: pattern, options: [])

```

次に、`matches`メソッドを使ってテキスト内でパターンにマッチする箇所を見つけることができます。

```Swift
let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))

for match in matches {
    let range = match.range
    let matchString = (text as NSString).substring(with: range)
    print(matchString) // "正規表現"
}
```

## ディープダイブ

正規表現の書式を学ぶことで、パターンをより詳細に指定することができます。例えば、`+`や`*`を使って特定の文字が1回以上または0回以上繰り返されることを表現できます。また、`[]`を使って複数の文字の中からマッチするものを選ぶことができます。

さらに、正規表現の辞書を用意することで、より複雑なパターンを探すことができます。これにより、複数のパターンを組み合わせたり、特定の文字列のグループを検索することができます。

## 参考リンク

- [Swiftの正規表現](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID289)
- [正規表現のチートシート](https://www.ibm.com/support/knowledgecenter/ja/SSMKHH_10.0.0/com.ibm.etools.mft.doc/be43230_.htm#be43230___5symbols)
- [オンライン正規表現テスター](https://regex101.com/)