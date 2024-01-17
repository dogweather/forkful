---
title:                "「文字列を小文字に変換する」"
html_title:           "Swift: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 何？なんで？

文字列を小文字に変換するとは、文字列内の文字をすべて小文字に変換することを指します。プログラマーがこれを行う理由は、入力データを統一的に扱うためです。例えば、ユーザーからの入力を受け取った際、大文字と小文字を区別せずに処理する必要がある場合があります。そのため、文字列を小文字に変換することで統一的に扱うことができます。

# 方法：

```Swift
let str = "Hello, World!"
print(str.lowercased())
```

出力結果: "hello, world!"

ここでは、定数`str`に"Hello, World!"という文字列を代入し、`lowercased()`メソッドを使用して文字列を小文字に変換しています。そして、`print`することで出力結果を確認することができます。

# 深く掘り下げる

## 歴史的背景

文字列を小文字に変換する方法は、古くから存在しています。昔のプログラミング言語では、大文字と小文字を区別するものが多く、文字列を扱う際には注意が必要でした。しかし、最近のプログラミング言語では、多くの場合大文字と小文字を区別しないよう設計されています。

## 代替手段

文字列を小文字に変換する方法は、`lowercased()`メソッド以外にもいくつかあります。例えば、文字列を全て大文字に変換する`uppercased()`メソッドや、最初の文字だけを大文字に変換する`capitalized`プロパティがあります。

## 実装の詳細

`lowercased()`メソッドは、実際には文字列のコピーを作成し、元の文字列を変更するのではなく、新しい文字列を返します。そのため、元の文字列に対して何らかの変更を行う場合は、新しい文字列を変数に代入し、それを操作する必要があります。

# 関連情報を参照：

Apple公式ドキュメント：https://developer.apple.com/documentation/swift/string/2894471-lowercased

Swiftドキュメンテーション：https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html