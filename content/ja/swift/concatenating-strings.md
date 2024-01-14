---
title:                "Swift: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの利点は、データをより効率的に扱えることです。例えば、フォーマットされたメッセージの作成や、複数の変数を結合してテキストを生成する際に便利です。

## 使い方

文字列を連結するには、以下のように`+`演算子を使います。

```Swift
let firstName = "Taro"
let lastName = "Yamada"
let fullName = firstName + lastName

print(fullName) // 出力結果：TaroYamada
```

文字列の連結には、データ型を揃える必要があります。例えば、`Int`型と`String`型を連結しようとすると、エラーが発生します。その場合は、`String()`を使ってデータ型を変換する必要があります。

```Swift
let age = 25
let message = "私の年齢は" + String(age) + "歳です。"

print(message) // 出力結果：私の年齢は25歳です。
```

## ディープダイブ

文字列の連結方法は複数あり、それぞれに長所や短所があります。例えば、`String()`を使用する代わりに、`\()`を使って文字列内に変数を埋め込むこともできます。また、文字列補間を使うことで、より簡潔かつ読みやすいコードを書くことができます。さらに、長い文字列を連結する際は、`join()`メソッドを使用することでパフォーマンスを向上させることができます。

## 参考リンク

- [公式ドキュメント: String Concatenation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID297)
- [Swift Strings and Characters](https://www.raywenderlich.com/165660/how-to-work-with-strings-in-swift)
- [Swift String Concatenation - Stack Overflow](https://stackoverflow.com/questions/26590056/swift-string-concatenation)