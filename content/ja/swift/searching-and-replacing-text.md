---
title:                "Swift: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ
テキストの検索と置換に取り組む理由は、プログラミングにおいて非常に重要なスキルだからです。例えば、大規模なコードベースで同じ変数名を一括で変更したい場合や、特定の単語を一括で置換したい場合など、様々な理由で検索と置換を行う必要があります。

## 方法
Swiftでは、 `replacingOccurrences(of:with:)` メソッドを使用することで簡単にテキストの検索と置換を行うことができます。このメソッドは、検索したい文字列を `of` パラメータに、置換したい文字列を `with` パラメータに渡すことで使用することができます。例えば、以下のようにコードを書くことで文字列内の `Hello` を `こんにちは` に置換することができます。

```Swift
let string = "Hello, World!"
let replacedString = string.replacingOccurrences(of: "Hello", with: "こんにちは")

print(replacedString)  // "こんにちは, World!"
```

## ディープダイブ
さらに、 `replacingOccurrences(of:with:)` メソッドでは、第3引数として `options` パラメータを使用することで検索の方法を指定することができます。例えば、 `caseInsensitive` を指定することで大文字と小文字を区別せずに検索を行うことができます。また、 `regex` を指定することで正規表現を使用した検索を行うこともできます。詳細な情報は公式ドキュメントを参照してください。

## 併せて読みたい
- [Swift公式ドキュメント - replacingOccurrences(of:with:options:)](https://developer.apple.com/documentation/foundation/nsstring/1409535-replacingoccurrences)
- [Swiftでテキストの検索と置換を行う方法](https://qiita.com/i_saint/items/36b5594ba0b0deeeb947)
- [Swiftで正規表現を使ってテキストを検索・置換する方法](https://qiita.com/motokiee/items/ff0e77708454f319ccba)