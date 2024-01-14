---
title:                "Gleam: サブストリングの抽出"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ？

テキスト処理をしている時に、しばしば部分文字列を抽出する必要があります。例えば、電話番号を処理する時に国番号や市外局番を取り除きたい場合などです。Gleamの組み込み関数である`String.substring`を使用すれば、簡単に部分文字列を抽出することができます。

## 方法

まず、`String.substring`を使用するためには、対象となる文字列と抽出する部分文字列の先頭と終端の位置を指定する必要があります。以下は電話番号から市外局番を抽出する例です。

```Gleam
let phone_number = "012-345-6789"
let area_code = String.substring(phone_number, 4, 6)
```

上記の例では、`String.substring`によって`phone_number`の4番目の位置から6番目の位置までの部分文字列が抽出され、`area_code`に格納されます。抽出された部分文字列は、`345`となります。

また、より複雑な抽出を行いたい場合は、正規表現を使用することもできます。正規表現には標準ライブラリの`Re`モジュールを使用します。

```Gleam
let email = "example@domain.com"
let username = Re.findall("(.+)@", email)
```

上記の例では、`"example"`が`username`に格納されます。

## ディープダイブ

`String.substring`は文字列を抽出するだけでなく、置換や削除も行うことができます。また、正規表現を使用することで、より複雑な文字列処理を行うことができます。詳細な使い方については、公式ドキュメントを参照してください。

## はじめに

Gleamの`String.substring`を使用することで、簡単に文字列の部分を抽出し、必要な処理を行うことができます。正規表現を組み合わせることで、さらに柔軟な文字列処理が可能になります。ぜひお試しください！

## 関連リンク

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [Gleamの正規表現チュートリアル](https://gleam.run/tutorials/regex/)