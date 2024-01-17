---
title:                "文字列の連結"
html_title:           "Swift: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何をするの？なぜするの？

文字列の連結とは、複数の文字列を一つの文字列にまとめることです。プログラマーがこれを行う理由は、文字列を合成してより複雑な文や、可変的な文を作成するためです。

## 使い方：

```Swift
let firstName = "太郎"
let lastName = "山田"

let fullName = firstName + lastName

print(fullName)

// 出力：太郎山田
```

```Swift
let greeting = "こんにちは"
let name = "太郎さん"

print(greeting + name)

// 出力：こんにちは太郎さん
```

## もっと詳しく：

文字列の連結は、プログラミングにおいて非常に一般的な操作です。古くからある言語では、文字列の連結には複雑な方法が必要でしたが、Swiftでは単純かつ直感的な方法でこれを行うことができます。

文字列の連結には、他にも便利な方法があります。文字列補間と呼ばれる方法では、変数や定数を文字列の中に埋め込むことができます。また、Swiftには高度な文字列操作ができるString型がありますので、より柔軟な文字列の合成が可能です。

## 関連リンク：

- [Swiftの文字列操作について](https://www.yamamanx.com/swift-string-format/)
- [文字列の連結の別の方法について](https://qiita.com/DaisukeKawamura/items/88ea91a284ed7aca1642)