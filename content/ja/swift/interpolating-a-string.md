---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列のエンベッドは変数、定数やリテラルなどの値を文字列に直接組み込むことです。プログラマがこれを行う理由は、コードをきれいに読みやすくするためと、表示用の文字列の更新や修正を容易に行うためです。

## 使い方:
以下にSwiftでの文字列のインターポレーションを示します。
```Swift
let studentName = "Kento"
let studentAge = 20
let studentInfo = "My name is \(studentName) and I'm \(studentAge) years old."
print(studentInfo)
```

上記のコードは次の出力を生成します。
```Swift
My name is Kento and I'm 20 years old.
```
変数に自由に値を設定でき、この方法を使用すると文字列内に値を動的に挿入できます。

## 深層探検 
文字列のインターポレーションは、Swiftが登場する以前から数多くの言語に実装されていました。しかし、Swiftはこの機能をより直感的で使いやすいものにアップグレードしました。代替策としては、文字列の連結やフォーマット指定文字列がありますが、これらはコードの見通しを悪くしたり、型安全性を損なう可能性があります。Swiftの文字列インターポレーションはこのような問題を解決し、型安全性を保ちつつ、見通しの良いコードを書くことが可能です。

## 参考資料
- Swift公式ドキュメント - 字句構造: [https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html](https://docs.swift.org/swift-book/ReferenceManual/LexicalStructure.html)
- スタックオーバーフロー- Swiftの文字列インターポレーション: [https://stackoverflow.com/questions/24051314/whats-the-string-format-specifier-for-a-string-in-swift](https://stackoverflow.com/questions/24051314/whats-the-string-format-specifier-for-a-string-in-swift)