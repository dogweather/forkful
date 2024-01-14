---
title:    "Swift: 文字列を小文字に変換する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

### シーク一覧

こんにちは、Swiftプログラマーの皆さん！今日は、文字列を小文字に変換する方法についてお話ししたいと思います。文字列を変換することは、プログラムの中で非常によく使用されるので、ぜひ覚えておいてください。

## なぜ？

文字列を小文字に変換する理由は、プログラムの中で大文字や小文字を区別せず扱うためです。例えば、ユーザーが大文字で入力した場合でも、プログラムが文字列を小文字に変換することで、正しい処理が行われるようになります。

## 方法

文字列を小文字に変換するには、`lowercased()`メソッドを使用します。以下のコードでは、"Hello, World!"という文字列を小文字に変換して出力しています。

```Swift
let myString = "Hello, World!"
let lowercaseString = myString.lowercased()
print(lowercaseString)

// Output: hello, world!
```

また、日本語の文字列でも同様に小文字に変換することができます。

```Swift
let myString = "こんにちは、世界！"
let lowercaseString = myString.lowercased()
print(lowercaseString)

// Output: こんにちは、世界！
```

## ディープダイブ

`lowercased()`メソッドは、文字列をすべて小文字に変換するだけでなく、アクセント付き文字を正しく処理したり、日本語の「平仮名」と「片仮名」を正しく変換することができます。

しかし、注意点としては、このメソッドは文字列を変更せず、新しい文字列を返すため、変換後の文字列を変数に代入する必要があります。

## 今後もっと詳しく学びたい方へ

[Swift ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)を参考に、文字列の操作についてもっと詳しく学ぶことができます。また、`lowercased()`メソッド以外にも、文字列を操作するためのさまざまなメソッドが用意されているため、ぜひ調べてみてください。さまざまなメソッドを使いこなすことで、より効率的なプログラムを作ることができるようになるでしょう。

### 関連リンク

- [Swiftドキュメント](https://docs.swift.org/swift-book/)
- [Swift言語入門 〜基本〜](https://qiita.com/_Daiki_/items/e5a01ab9d20b227f97e2)
- [【初心者向け】Swiftの基礎まとめ](https://qiita.com/usagiramen/items/f1531fdd4ea42756e597)