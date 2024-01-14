---
title:                "Swift: 「文字列の連結」"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

＃＃なぜ
Swiftで文字列を連結するのに人々が参加する理由をご紹介します。文字列を連結することで、より簡潔で読みやすいコードを書くことができます。

＃＃方法
まずは、concatenate（連結）メソッドを使う基本的な方法をご説明します。

`` `Swift
let firstName = "太郎"
let lastName = "山田"
let fullName = firstName + lastName // fullNameは "太郎山田"となります
`` ```

次に、文字列補間（String interpolation）を使って連結する方法をご紹介します。

`` `Swift
let age = 20
let message = "私の年齢は\(age)歳です。" // messageは "私の年齢は20歳です。"となります
`` ```

また、文字列を連結する際に配列を使っても簡単にできます。

`` `Swift
let numbers = [1, 2, 3]
let result = numbers.map { String($0) }.joined(separator: "、") // resultは "1、2、3"となります
`` ```

＃＃深く掘り下げる
Swiftでは、かつては加算演算子（+）を使って文字列を連結していましたが、Swift 5からは改良された連結メソッドを採用しています。これにより、文字列の連結のパフォーマンスが向上し、メモリーの節約にもつながります。

また、文字列補間や配列を使って連結する方法も、拡張性や可読性の面でより優れていると言えます。しかし、大量の文字列を連結する処理では、性能面で影響が出てくる場合もあるので注意が必要です。

＃＃参考リンク
- [Swift 公式ドキュメント - Concatenating Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID264)
- [文字列を連結する方法 【Swift入門】](https://beyondcoach.net/post-3086)
- [Apple Developer Forum - String Interpolation vs Concatenation](https://developer.apple.com/forums/thread/141178)