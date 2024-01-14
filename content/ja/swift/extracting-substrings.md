---
title:                "Swift: 「部分文字列の抽出」"
simple_title:         "「部分文字列の抽出」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

Swiftでサブストリングを抽出する必要があるのでしょうか？サブストリングを抽出することで、文字列をより細かく分割することができます。例えば、名前や住所など、文字列の一部を取り出す場合に便利です。

## 方法

まずは、どの文字列からサブストリングを抽出するかを決めます。次に、どの部分を抽出したいかを指定します。最後に、サブストリングを抽出するメソッドを使って実際に抽出します。例えば、以下のようなコードを使うことができます。

```Swift
let fullName = "Yuto Yamada"
let firstName = fullName.prefix(4) // 「Yuto」が抽出される
```

抽出する部分を指定する際には、文字列のインデックスを使います。例えば、上記の例では「Yuto」の最初の文字、つまりインデックス番号が0の文字から４文字分を抽出しています。抽出する部分がどのインデックス番号に該当するかを把握することが重要です。

## 深堀り

サブストリングを抽出するメソッドには、いくつかの種類があります。それぞれのメソッドは、抽出する部分の指定方法や戻り値の形式が異なります。例えば、下記のようなメソッドがあります。

- `prefix`: 文字列の先頭から指定した数の文字を抽出する
- `suffix`: 文字列の末尾から指定した数の文字を抽出する
- `dropFirst`: 指定した数の文字を先頭から削除した文字列を返す
- `dropLast`: 指定した数の文字を末尾から削除した文字列を返す

また、`string.index()`メソッドを使うことで、指定したインデックス番号の文字を取得することもできます。

## 参考リンク

- [Swiftでの文字列操作について](https://developer.apple.com/documentation/swift/string)
- [サブストリングを抽出する方法](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string-coded-in-swift)
- [文字の抽出と削除に関するチュートリアル](https://www.swiftmania.io/tutorials/learn-how-to-extract-remove-characters-from-strings-in-swift)
- [文字列の処理に関する基本的なメソッド一覧](https://qiita.com/KangKiBeom/items/64721003b9390c991fd1)


## 関連リンク