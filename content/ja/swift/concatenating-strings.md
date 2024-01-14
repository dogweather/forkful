---
title:    "Swift: 文字列の連結"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

今回の記事では、Swiftで文字列を連結する方法について説明します。文字列を連結すると、プログラムの柔軟性を高め、より読みやすいコードを書くことができます。

## 方法

文字列を連結するには、以下のように`+`演算子を使用します。

```Swift
let string1 = "こんにちは"
let string2 = "世界"
let result = string1 + string2
print(result)
```

このコードを実行すると、`こんにちは世界`という出力が得られます。また、文字列の中に他の変数の値を挿入することもできます。

```Swift
let name = "太郎"
let greeting = "こんにちは、\(name)さん"
print(greeting)
```

この場合、出力は`こんにちは、太郎さん`となります。

## 詳しい情報

文字列を連結するには、`+`演算子以外にも`append()`メソッドや`concat()`関数などがあります。また、文字列の中に数字や複数の変数を挿入する際には、フォーマット指定子を使用することもできます。詳しくは公式ドキュメントをご覧ください。

## See Also

- [Swift 公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [文字列を連結する方法](https://www.tutorialspoint.com/swift/swift_concatenation.htm)