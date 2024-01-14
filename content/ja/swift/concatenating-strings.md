---
title:    "Swift: 文字列の連結"
keywords: ["Swift"]
---

{{< edit_this_page >}}

### なぜ？

文字列の連結を行う理由は、プログラムで複数の文字列を結合してより長い文字列を作成する必要があるからです。たとえば、ユーザー名とパスワードを組み合わせてログイン認証を行う場合などに使用されます。

### 方法

```Swift
let userName = "Emily"
let password = "p@ssw0rd"
let loginMessage = "Welcome, " + userName + "! Your password is " + password
print(loginMessage)
```

```
出力：Welcome, Emily! Your password is p@ssw0rd
```

### 詳細を調べる

文字列の連結には、`+`や`+=`などの演算子を使用する方法以外にも、`String`クラスの`append()`や`join()`などのメソッドを使用する方法もあります。また、文字列のフォーマットに応じて、`String`クラスの`format()`メソッドを使用することもできます。

### See Also

[Swiftの基礎](https://developer.apple.com/jp/swift/blog/articles/introducing-swift/)  \
[Stringクラスのドキュメント](https://developer.apple.com/documentation/swift/string) \
[Stringのフォーマット方法について](https://learnappmaking.com/swift-string-formatting-how-to/#concatenatingstrings)