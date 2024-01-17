---
title:                "テキストの検索と置き換え"
html_title:           "Swift: テキストの検索と置き換え"
simple_title:         "テキストの検索と置き換え"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 何& なぜ？
テキスト検索と置換とは、プログラマーがコードの中で特定の文字列を探し、別の文字列に置き換えることを指します。プログラマーがテキストの検索と置換を行う理由は、コードの修正や変更を効率的に行うためです。

## 方法：
以下の ```Swift ... ``` コードブロックに示したコーディング例と出力を参考にしてください。

```swift
// 検索して置換する例
let sentence = "今日はいい天気です。"
let newSentence = sentence.replacingOccurrences(of: "いい", with: "素晴らしい")
print(newSentence) // 出力：今日は素晴らしい天気です。
```

```swift
// 複数の文字列を置換する例
let codingLanguages = ["Swift", "Objective-C", "Java", "Kotlin"]
let newCodingLanguages = codingLanguages.map { $0.replacingOccurrences(of: "C", with: "++") }
print(newCodingLanguages) // 出力：["Swift", "Objective++", "Java", "Kotlin"]
```

## 深堀り：
以下の内容を参考に、テキストの検索と置換についてさらに詳しく学んでください。

1. 過去の文脈：テキストの検索と置換は古くから使われている技術であり、プログラミング言語だけでなく、テキストエディタの機能でもよく使われています。
2. 代替手段：テキストの検索と置換には、プログラミング言語の標準関数だけでなく、正規表現などの機能を使った高度な方法もあります。
3. 実装の詳細：Swiftでは、テキストの検索と置換に```String```クラスの```replacingOccurrences```メソッドを使うことができます。このメソッドには、テキストの一部分をパターンとして指定し、それを別の文字列に置換する機能が備わっています。

## 関連情報：
テキストの検索と置換についてのさらなる情報を知りたい方は、以下のリンクを参考にしてください。

- [Swift公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [正規表現の使い方](https://qiita.com/shizuma/items/22c69371afe51672c6f2)
- [テキストエディタの検索と置換](https://support.apple.com/kb/PH18638?locale=ja_JP&viewlocale=ja_JP)