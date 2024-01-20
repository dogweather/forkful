---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Swiftでパターンに一致する文字を削除する

## 何となぜ？

パターンに一致する文字の削除とは、特定の文字列パターンを見つけてそれを削除することです。これは無駄なスペースを取り除いたり、不要な記号を削除したりするために行います。

## ハウツー：

以下の例では、ある文字列から全ての数字を削除する方法を示します。

```Swift
let originalString = "Hello123World456"
let removedDigits = originalString.filter { !("0"..."9").contains($0) }
print(removedDigits)
```

出力は以下のようになります。

```Swift
HelloWorld
```

## ディープダイブ：

この操作の歴史的な文脈によって、RegExpまたは正規表現がしばしばこのタスクに使われます。しかしSwiftでは、`filter`関数と文字の範囲を使った方法が提供されています。

代替手段としては、正規表現を使用する方法がありますが、その場合はより複雑なパターンの一致を扱うことが可能になります。

具体的な実装については、`filter`関数は配列または集合の各要素に対してテストを行い、そのテストをパスした要素だけを新しい配列に追加します。この場合、テストは指定した範囲内に文字が存在するかどうかです。

## 参考資料：

* [Swiftの公式ドキュメンテーション](https://developer.apple.com/documentation/swift)
* [SwiftのStringの操作](https://www.hackingwithswift.com/articles/141/8-examples-of-swift-strings)
* [正規表現についての詳細](https://developer.apple.com/documentation/foundation/nsregularexpression)