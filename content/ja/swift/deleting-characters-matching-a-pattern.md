---
title:                "Swift: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字を削除するのは、プログラミングにおいて特定のパターンにマッチする文字を処理する必要がある場合に最適です。

## 方法

文字を削除するためには、Swiftの組み込み関数`removeAll()`を使用します。以下のコードを参考にしてください。

```Swift
let sentence = "私の名前は太郎です。"
let filteredSentence = sentence.removeAll { $0 == "私" }
print(filteredSentence)
```

このコードの出力は「の名前は太郎です。」となります。`removeAll()`関数は、与えられた文字とマッチする場合に文字を削除するため、特定のパターンを持つ文字列を処理する非常に有用なツールです。

## ディープダイブ

`removeAll()`関数を使用する際には、いくつかの注意点があります。まず、この関数はマッチする文字を見つけ次第即座に削除するため、文字列内の文字の順序が重要です。また、文字列内にマッチする文字が複数ある場合、全てのマッチする文字を同時に削除します。さらに、今回の例では1文字に限定していますが、複数の文字を指定することも可能です。

## その他の情報

もし文字を削除する際により複雑なパターンを処理する必要がある場合は、正規表現を使用することができます。しかし、正規表現は複雑な構文を持つため、注意して使用する必要があります。

## 参考リンク

- [Swiftでの文字列処理の基礎](https://www.raywenderlich.com/3-string-handling-in-swift)
- [正規表現の基礎](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swiftの組み込み関数`removeAll()`について](https://developer.apple.com/documentation/swift/string/2952960-removeall)