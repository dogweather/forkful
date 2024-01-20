---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何であり、なぜ? (WHAT & WHY?)

テキストの検索および置換とは、特定の文字列を見つけてそれを別のものに変えることです。プログラマがこれを行う理由は、大量のデータを迅速に処理するため、または特定種類のエラーを直すために通常です。

## どうやって (HOW TO)

以下にSwiftでのテキスト検索と置換の基本的なやり方を示します。これはあくまで基本的なやり方ですが、一般的なユースケースで十分に使えます。

```Swift
var string = "Hello, Swift"
string = string.replacingOccurrences(of: "Swift", with: "World")
print(string)  // prints "Hello, World"
```

上記のコードは、"Swift"という文字列を"World"に置換して出力します。

## ディープダイブ (DEEP DIVE)

テキストの検索および置換は計算の歴史と密接に関連しています。なぜなら、初期のコンピュータシステムがテキスト操作が中心だったからです。

Swiftでは、`replacingOccurrences(of: with:)`がもっとも一般的な方法ですが、ある特定の条件に合うものだけを置換したい場合などには、正規表現を使う方法もあります。詳しくは、Swiftのドキュメンテーションを参照してください。

この`replacingOccurrences`メソッドは、内部的には全体の文字列をスキャンすることで実現しています。つまり、このメソッドの時間複雑度はO(n)で、nは文字列の長さを表します。

## 関連資料 (SEE ALSO)

- Swiftの公式ドキュメンテーション: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Regular Expressions in Swift](https://benscheirman.com/2014/06/regex-in-swift/)
- [WWDC 2019](https://developer.apple.com/videos/play/wwdc2019/265/)