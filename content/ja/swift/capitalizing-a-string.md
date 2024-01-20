---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を大文字にするっていうのは、例えば「hello」を「HELLO」に変換することだよ。これをする理由は、タイトルやユーザー入力の正規化、そして視認性の向上のためだね。

## How to: (方法)
```Swift
// Swiftでの文字列を大文字に変換する方法
let lowercaseString = "hello, japan!"
let uppercaseString = lowercaseString.uppercased()

print(uppercaseString)  // 出力: "HELLO, JAPAN!"
```

## Deep Dive (掘り下げ)
文字列を大文字に変換するのは、プログラミングにおいてよく使用される操作の一つ。Swift では `uppercased()` 関数を使うことでこれを簡単に行うことができる。

歴史的には、古いコンピューターシステムやプログラミング言語では大文字と小文字を区別しないことが多かった。しかし、今日では大文字と小文字を明確に区別し、それぞれが異なる意味を持つことが一般的だ。

Swift以外にも、他の言語で似たような機能がある。例えば、JavaScript では `toUpperCase()`、Python では `upper()` と書く。違いはあるけど、基本的なアイデアは同じだね。

実装の詳細としては、`uppercased()` 関数はUnicodeをサポートしている。だから、様々な言語の特殊な文字も正しく大文字に変換されるよ。

## See Also (関連情報)
- Swiftの公式ドキュメント: [String](https://developer.apple.com/documentation/swift/string)
- Unicodeについての詳細: [Unicode.org](http://unicode.org/)
- 他言語の文字列大文字化機能比較: [Wikipedia - Case Conversion](https://en.wikipedia.org/wiki/Letter_case#Case_conversion)