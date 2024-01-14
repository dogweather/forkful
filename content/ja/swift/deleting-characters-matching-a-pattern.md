---
title:    "Swift: パターンに合致する文字の削除"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
「特定のパターンに一致する文字を削除する」という作業をするには、文字列処理が必要になる可能性があります。もしくは、単にテキストをクリーンアップしたい場合に役立ちます。

## 方法
```Swift
let string = "Hello World!"
let newString = string.replacingOccurrences(of: "o", with: "")
print(newString)
```
出力：Hell Wrld!

このように、 `replacingOccurrences` 関数を使用して、指定した文字を別の文字で置き換えることができます。

## 深層ダイブ
文字列処理についてさらに詳しく学ぶには、正規表現を使用することができます。正規表現を使用することで、より複雑なパターンに一致する文字を効率的に削除することができます。

## 参考
- [Swift公式ドキュメント](https://developer.apple.com/documentation/foundation/nsstring/1412441-replacingoccurrences)
- [正規表現入門](https://www.atmarkit.co.jp/ait/articles/1308/02/news010.html)
- [正規表現チートシート](https://www.cisco.com/c/ja_jp/support/docs/security/email-security-appliance/118654-technote-esa-00.html)