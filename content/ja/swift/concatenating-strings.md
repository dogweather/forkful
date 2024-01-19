---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？
"文字列の連結"は、2つ以上の文字列を1つに結合するプログラミング技術です。プログラマーはこれを使って、情報を組み合わせて新しいメッセージを生成したり、ユーザーに表示する出力を動的に生成するために、使用します。

## 方法：
Swiftには複数の文字列の連結を容易にするためのさまざまな方法があります。

```Swift
// "+" 演算子を使用
let string1 = "こんにちは, "
let string2 = "世界!"
let message = string1 + string2
print(message) // 結果： "こんにちは, 世界!"

// 複合代入演算子 "+=" を使用
var welcome = "こんにちは, "
welcome += "世界!"
print(welcome) // 結果： "こんにちは, 世界!"

// String Interpolation を使用
let name = "友達"
print("こんにちは, \(name)!") // 結果： "こんにちは, 友達!"
```

## 深いダイブ：
文字列の連結はコンピューティングの歴史の初期から存在し、その実装詳細と代替手段は言語によって大きく異なります。

1. 歴史的な文脈：初期のコンピューティングではメモリが非常に制限されており、文字列の連結は稀でコストが高い操作でした。しかし、現代のシステムではこれは大きな問題ではありません。

2. 代替手段：ほとんどの言語には、スピード、メモリ使用量、または構文の明確さを改善するための代替的な文字列連結手段があります。Swiftでは、"+"演算子、"+="複合代入、または文字列補間を用いて行うことができます。

3. 実装詳細: "+"演算子と"+="は文字列の連結に一般的に使用されますが、大量の文字列を連結する場合はその効率が落ちます。これを解決するために、Swiftでは文字列補間が推奨されます。

## 関連リンク：
1. 公式ドキュメンテーション: [Swift公式ドキュメンテーション](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. Swift の文字列連結についての深いダイブ : [String Concatenation In Swift](https://www.hackingwithswift.com/articles/162/how-to-use-string-interpolation-in-swift)