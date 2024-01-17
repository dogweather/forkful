---
title:                "特定のパターンに一致する文字を削除する"
html_title:           "Swift: 特定のパターンに一致する文字を削除する"
simple_title:         "特定のパターンに一致する文字を削除する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何をしているの？ (What & Why?)
文字列内で特定のパターンに一致する文字を削除するとは、プログラマーが特定の文字を手動で削除する代わりに、より効率的に複数の文字を一度に削除することを意味します。プログラマーは、コードをより洗練されたものにするために、または文字列の処理をより簡単にするために、文字列内の不要な文字を削除する必要があります。

## やり方 (How to:)
文字列内で特定のパターンに一致する文字を削除するには、正規表現を使用します。以下のコードを参考にしてください。

```Swift
let str = "Hello World!"
let pattern = "[aeiou]"
let result = str.replacingOccurrences(of: pattern, with: "", options: .regularExpression)
print(result)
```

このコードは、文字列"Hello World!"から母音を削除し、"Hll Wrld!"という結果を出力します。

## 深堀り (Deep Dive)
文字を削除する方法はいくつかありますが、正規表現は最も一般的で効率的な方法です。正規表現は、規則的なパターンを指定し、そのパターンに一致する文字を一度に複数削除することができます。また、文字列内で特定の文字を検索し、置換することもできます。

他の代替方法としては、ループを使用して文字を一つずつチェックし、不要な文字を削除する方法があります。しかし、この方法は複雑で時間がかかります。

## 関連リンク (See Also)
- [正規表現チートシート](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Apple Developer Documentation - 正規表現](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [正規表現を使用した文字列処理の例](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)