---
title:                "文字列の補間"
html_title:           "Swift: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## なに？なぜ？
文字列の補間とは何かを説明する時間です。プログラマーがそのような作業をする理由もお話ししましょう。

文字列の補間とは、変数や定数の値を文字列の中に埋め込むことです。例えば、"私の名前は\(name)です"という文字列の中に、実際の名前を代入することができます。これにより、繰り返し使用される文字列を簡潔に作成できるようになります。プログラマーがこの作業をする理由は、コードをより読みやすく、効率的にするためです。

## やり方：
以下のコードブロック内に、コード例と実際の出力を記載しています。

```Swift
let name = "John"
print("私の名前は\(name)です")
// 出力：私の名前はJohnです
```

文字列補間では、バックスラッシュと丸括弧を使用して、変数や定数を文字列の中に埋め込みます。このように、コード内に変数や定数の値を直接記述することなく、文字列を動的に作成することができます。

## 深く掘り下げる：
文字列補間の歴史的背景や代替手段、実装の詳細についてお話しします。

文字列補間は、Swiftの前身であるObjective-Cから受け継がれた機能です。Objective-Cでは、文字列の中に変数や定数を埋め込む際に、プレースホルダーとして%を使用していました。しかし、Swiftでは記法が変更され、より簡単に変数や定数を文字列に埋め込むことができるようになりました。

文字列補間の代替手段としては、文字列連結やフォーマット指定子、文字列フォーマット化メソッドなどがあります。しかし、文字列補間を使用すれば、より簡潔で読みやすいコードを作成できるでしょう。

## 関連情報：
文字列補間についてさらに学ぶための参考文献をリンクします。

- [Appleのドキュメント - 文字列補間](https://developer.apple.com/documentation/swift/string_interpolation)
- [Swiftレシピ - 文字列補間](https://swift-ref.herokuapp.com/string-interpolation)
- [【実践Swift】文字列を簡単に作成する方法 - 文字列補間](https://dev.classmethod.jp/articles/swift-string-interpolation/)