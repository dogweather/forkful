---
title:    "Swift: テキストの検索と置換"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ

プログラミングを行う際、テキストの検索や置換を行うことは非常に重要です。それにより、コードの効率性や正確さを向上させることができます。

## 方法

テキストの検索や置換を行う方法はいくつかありますが、その中でもSwiftの特徴である高速かつ簡単な方法を紹介します。まずは以下のように、文字列を変数に代入します。

```Swift
let text = "プログラミングは楽しいです"
```

次に、文字列の中から「楽しい」を「楽しくない」に置換するコードを書いてみましょう。

```Swift
let newText = text.replacingOccurrences(of: "楽しい", with: "楽しくない")
print(newText)
```

このコードを実行すると、コンソールに「プログラミングは楽しくないです」という結果が表示されます。

## 深堀り

上記のコードでは、単純に文字列の中から特定の単語を置換していますが、実際にはもっと複雑な検索や置換が必要になることもあります。そのためには、正規表現を使用することができます。正規表現とは、文字列のパターンマッチングを行うための表記法です。

以下のコードでは、文字列中の全ての数字を「0」に置換する例を示しています。

```Swift
let text = "Swiftは3つのバージョンをリリースしました"
let regex = try! NSRegularExpression(pattern: "[0-9]", options: NSRegularExpression.Options())
let newText = regex.stringByReplacingMatches(in: text, options: [], range: NSMakeRange(0, text.count), withTemplate: "0")
print(newText)
```

コンソールに「Swiftは0つのバージョンをリリースしました」という結果が表示されます。正規表現を使うことで、より柔軟な検索や置換を行うことができるようになります。

## 関連情報

- [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Stringの検索と置換](https://qiita.com/Shino_t0m9r/items/970e9aa1876a07aea7f7)
- [Swiftで正規表現を利用する方法とTips](https://qiita.com/chorook/items/9a03a222c3700694a4d5)