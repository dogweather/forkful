---
title:    "Swift: テキストの検索と置き換え"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

あなたがテキストを検索して置換する必要があるかもしれません。検索して置換することで、複数の文字列を効率的に変更することができます。これは、大規模なプロジェクトや複数のファイルで同じテキストを使用する場合に特に役立ちます。

## なぜ

テキストを検索して置換することで、手動で複数の箇所を変更する作業を省くことができます。これにより、時間と労力を節約することができます。

## 方法

検索して置換する方法は非常に簡単です。まず、`replaceOccurrences(of:with:)`メソッドを使用して、変更したい文字列と変更後の文字列を指定します。次に、`options`パラメーターに`[]`を渡し、検索と置換を行う範囲を指定します。最後に、`replacingOccurrences(of:with:options:)`メソッドを使用して、実際に置換を行います。

例えば、次のようにコードを書くことで、全ての"Hello"を"こんにちは"に置換することができます。

```Swift
let originalString = "Hello, Swift!"
let newString = originalString.replacingOccurrences(of: "Hello", with: "こんにちは", options: [])
print(newString)

// Output:
// こんにちは, Swift!
```

## 詳細を調べる

検索して置換を行う時には、パターンマッチングを使用することもできます。これにより、より柔軟な検索が可能になります。例えば、次のように正規表現を使用して、全ての数字を"*"に置換することができます。

```Swift
let originalString = "1234abc"
let regex = try NSRegularExpression(pattern: "[0-9]", options: [])
let newString = regex.stringByReplacingMatches(in: originalString, options: [], range: NSRange(location: 0, length: originalString.count), withTemplate: "*")
print(newString)

// Output:
// ****abc
```

さらに、複数の条件を指定することもできます。例えば、次のように複数の単語を検索して一つの単語に置換することができます。

```Swift
let originalString = "The sky is blue and the grass is green."
let replacements = ["sky": "ocean", "grass": "field"]
for (word, replacement) in replacements {
    let regex = try NSRegularExpression(pattern: word, options: [])
    let newString = regex.stringByReplacingMatches(in: originalString, options: [], range: NSRange(location: 0, length: originalString.count), withTemplate: replacement)
}
print(newString)

// Output:
// The ocean is blue and the field is green.
```

## もっと詳しく知りたい場合は

検索して置換に関する詳細を知りたい場合は、以下のリンクを参考にしてください。

- [Apple Developer Documentation: String and Text](https://developer.apple.com/documentation/swift/string_and_text)
- [NSHipster: Regular Expressions](https://nshipster.com/nsregularexpression/)