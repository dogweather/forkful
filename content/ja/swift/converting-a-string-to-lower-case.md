---
title:                "Swift: 「文字列を小文字に変換する」"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する必要があるのは、プログラム内で大文字と小文字を区別しなければならない場合や、ユーザーからの入力を同一視する必要がある場合があります。

## 使い方

```Swift 
let string = "Hello World"
let lowercasedString = string.lowercased()
print(lowercasedString)
```

```
output: hello world
```

## 詳しく調べる

文字列を小文字に変換する方法はいくつかありますが、一番簡単な方法は`lowercased()`メソッドを使用することです。このメソッドは文字列の全ての文字を小文字に変換して新しい文字列を返します。

また、String型には`uppercased()`メソッドもあり、これは全ての文字を大文字に変換することができます。これらのメソッドは大文字と小文字を区別するだけでなく、非アルファベット文字も変換することができます。

さらに、`localizedLowercase`と`localizedUppercase`というプロパティを使うことで、使用している言語や地域の慣習に従って文字を変換することができます。

## 併せて参照

- [String - Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- [Converting between string cases using the Swift standard library - Hacking with Swift](https://www.hackingwithswift.com/example-code/strings/converting-between-string-cases-using-the-swift-standard-library)
- [ローカライズする - Swift: 第1回　文字列のローカル化 - プログラムはメディアだ](https://www.proglearn.com/swift-localization-string/)