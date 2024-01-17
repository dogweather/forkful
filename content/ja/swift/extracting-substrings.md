---
title:                "サブストリングの抽出"
html_title:           "Swift: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## これは何ですか？
文字列から一部分を取り出すことを抽象と呼びます。プログラマーがこれをする理由は、文の一部や特定の文字列を必要なときに取得したいからです。

## 方法：
Swiftでは、```Swift substring(from: Int) ```または```Swift substring(to: Int) ```を使用して、必要な部分文字列を取得することができます。例えば、
```Swift
let str = "こんにちは!"
let substr = str.substring(to: 5)
print(substr)  

// 出力結果：こんにちは
```

## 深堀り：
文字列から部分文字列を取り出すことの歴史的な文脈や代替方法については、正規表現やsubstring(with: Range\<String.Index>)の使用も考えられます。実装の詳細については、SwiftのStringクラスのドキュメントを参照してください。

## 関連サイト：
[Swiftのsubstringメソッドのドキュメント](https://developer.apple.com/documentation/swift/string/2927574-substring)