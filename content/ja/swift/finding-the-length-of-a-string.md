---
title:                "文字列の長さを見つける"
html_title:           "Swift: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何が何故必要なのか？

文字列の長さを求めるとは、文字列に含まれる文字符号の数を数えることです。プログラマーがこれを行う理由は、与えられた文字列の長さを知ることで、より複雑なプログラムを作るために必要なデータを得ることができるからです。

## 方法：

```
Swift let str = "こんにちは、世界！"
print(str.count)
```
上記のコードを実行すると、出力結果は`11`となります。文字列の長さは、ドット演算子を使って`count`プロパティにアクセスすることで取得することができます。

## 詳細を深く掘り下げる：

1. 歴史的文脈：文字列の長さを求めるという概念は、古典的なプログラミング言語から派生したものでした。今日、ほとんどのプログラミング言語は、文字列の長さを求めるための組み込み関数やメソッドを提供しています。
2. 代替手段：文字列の長さを求めるためには、他の方法もあります。例えば、文字列を反復処理してカウントする方法などが挙げられますが、これらの方法は低効率であり、バグの発生もより一般的です。そのため、組み込みの`count`プロパティを使うことが推奨されます。
3. 実装の詳細：Swiftの`String`型には、`count`プロパティが組み込まれているため、文字列の長さを取得することができます。そのため、追加のコードオーバーヘッドを心配する必要はありません。

## 関連リソース：

- [Swift公式ドキュメント：文字列](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [qiita.com：Swiftで文字列の長さを求める](https://qiita.com/t2psyto/items/d513bb7a19001c8ad1c9)