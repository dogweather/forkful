---
title:                "文字列から引用符を削除する"
date:                  2024-01-26T03:43:09.585369-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から引用符を削除するということは、コンテンツを囲んでいる引用符を取り除くことを意味します。これを行う理由としては、入力のサニタイズ、データの保存の準備、またはデータ処理に干渉する可能性のある不要なテキストフォーマットを取り除くためです。

## 方法：

Swiftでは、引用符を取り除く作業をかなり手際よく対応できます。`replacingOccurrences(of:with:)`を使用した素早い例を以下に示します。これは、文字通り、テキストの一部を別のもの、または何もないものと交換することを意味します。

```swift
var quotedString = "\"これは'引用'された文字列です。\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // これは'引用'された文字列です。

// シングルクォートを扱いますか？検索語を変更するだけです。
quotedString = "'こちらは別の例です。'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // こちらは別の例です。
```

出力されるのは、引用符のない文字列で、次に計画している何かにそのまま使えます。

## 詳細な調査

プログラミングの黎明期から、私たちはこういった文字列の「クリーンアップ」を行ってきました。初期には、貴重なメモリを節約し、入力を処理する際の構文エラーを避けることが主な理由でした。しかし、今日では、特にJSONを扱う際やデータベース作業のための文字列を準備する際に、良好なデータ衛生が重視されます。一つの引用符がSQLクエリを「構文エラー」と言う間もなく混乱させてしまうこともあります。

代替手段は？もし`replacingOccurrences(of:with:)`が少々平凡に感じられるなら、より複雑なパターンを扱う場合や、特定の位置の引用符のみを削除したい場合に正規表現への深掘りを検討するかもしれません。ここでの友達はSwiftの`NSRegularExpression`クラスです。しかし、正規表現は二刃の剣であることを覚えておいてください—強力ですが、時には過剰です。

実装に関しては、`replacingOccurrences(of:with:)`はSwiftの`String`によって提供されるメソッドで、内部ではUnicodeや現代のテキスト処理の他の複雑な部分を扱うより複雑な文字列操作関数を呼び出します。これはSwiftがあなたのために扱ってくれる、表面上はシンプルだが内部では複雑なプロセスです。

## 参照

Swiftにおける文字列操作の詳細については：

- Swift プログラミング言語 (文字列と文字)：[Swift.org の文書](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression：[Apple デベロッパー 文書](https://developer.apple.com/documentation/foundation/nsregularexpression)

それに、正規表現に興味が湧いたなら、あなたのパターンをテストしたい場合：

- Regex101：[Regex テスターとデバッガー](https://regex101.com)