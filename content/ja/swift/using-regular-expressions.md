---
title:    "Swift: 正規表現を使用する"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

**Why: なぜ正規表現を使用すべきなのか？**

正規表現は文字列を操作するための強力なツールです。文字列を検索したり、置換したり、一致するパターンを確認したりする際に便利です。また、パターンを定義することで、複雑な文字列を簡単に処理できます。

**How To: 正規表現の使用方法**

正規表現を使用するには、まずパターンを定義する必要があります。例えば、任意の数字の連続を表すパターンは「\d+」となります。次に、指定したパターンに対して操作を行います。以下は、文字列中から数字の連続を検索し、置換するコードの例です。

```Swift
let str = "今日は2月14日です"
let pattern = "\\d+"
let replacement = "12月25日"
let newStr = str.stringByReplacingOccurrences(of: pattern, with: replacement)
print(newStr) // 出力: "今日は12月25日です"
```

**Deep Dive: 正規表現の詳細**

正規表現パターンには様々な特殊文字が含まれています。例えば、「\d」は数字を表し、「.」はどの1文字にも一致するという意味です。また、「+」や「*」などの量指定子を使用することで、繰り返しパターンを定義することができます。さらに、グループ化やキャプチャなどの機能もあり、より柔軟なパターンの定義が可能です。

**See Also: 関連リンク**

- [正規表現チートシート](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Swift正規表現ガイド](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [正規表現エディター](https://regex101.com/)