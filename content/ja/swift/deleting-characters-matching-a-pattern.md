---
title:    "Swift: パターンに一致する文字を削除する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# なぜ

文字列のパターンにマッチする文字を削除することの利点は、コードをより効率的にし、より読みやすくすることです。

## 方法

```Swift
func deletePattern(text: String, pattern: String) -> String {
    return text.replacingOccurrences(of: pattern, with: "", options: .regularExpression)
}

// サンプル入力
let text = "Hello world! Goodbye world!"
let pattern = "[aeiou]" // 削除するパターンは母音

print(deletePattern(text: text, pattern: pattern))

// サンプル出力
Hll wrld! Gdb wrld!
```

## 深く掘り下げる

パターンにマッチする文字を削除する方法は、正規表現を使用して文字列を置換することで実現できます。`replacingOccurrences`メソッドには、削除するパターンを指定する`options`パラメーターを追加できます。ここでは、`regularExpression`オプションを使用しました。

# 参考リンク

- [SwiftのreplacingOccurrencesメソッド](https://developer.apple.com/documentation/foundation/nsstring/1417153-replacingoccurrences)
- [正規表現のパターン作成方法](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID311)