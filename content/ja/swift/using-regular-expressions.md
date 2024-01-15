---
title:                "正規表現を使用する"
html_title:           "Swift: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ使うのか

正規表現を使うことの利点は、テキストからパターンを検索したり、特定の形式に合う文字列を取り出したりすることができることです。

## 使い方

まずは、Swiftで正規表現を使うために必要なインポートをします。以下のコードは、単語のリストから「cat」が含まれるものを見つけ出す例です。

```Swift
import Foundation

let words = ["cat", "dog", "bird", "catfish", "lion"]

for word in words {
    if let range = word.range(of: "cat") {
        print("\(word) contains 'cat'")
    }
}
```
上記のコードを実行すると、以下の出力が得られます。

```
cat contains 'cat'
catfish contains 'cat'
```

正規表現パターンには、「cat」の他にも「c[a-z]+t」といった表現を使うこともでき、より柔軟な文字列の検索が可能になります。

## 応用情報

正規表現には、さまざまなオプションや特殊な記号があります。例えば、大文字と小文字を区別しないようにするオプションや、グループ化して文字列を取り出す方法もあります。詳細な情報は、オンラインドキュメントや書籍を参考にしてください。

## 参考リンク

- [Swift Regular Expressions - NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [正規表現入門 (1) マッチさせる・パターンを記述する (基本編) | bermuda](https://www.bermuda-labo.com/regexp/)
- [Swiftで正規表現を使おう！](https://qiita.com/shiz/items/085d2344d64189643c2f)