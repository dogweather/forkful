---
title:                "Gleam: テキストの検索と置換"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# なぜGleamでテキストの検索と置換を行うのか

初心者からプロまで、ほとんどのプログラマーは書いたコードを繰り返し修正する必要があります。しかし、手作業でテキストを探して置換するのは非効率的であり、ヒューマンエラーも起こりえます。そこで、Gleamプログラミング言語が役に立ちます。Gleamはパターンマッチングや文字列操作などの様々な機能を提供しており、テキストの検索と置換をより簡単にすることができます。

## Gleamでのテキストの検索と置換方法

まずはGleamのモジュールをインポートしましょう。

```Gleam
import gleam/string
```

次に、`gleam/string`モジュールの`replace`関数を使用して、テキストの検索と置換を行います。例えば、文字列の中から"hello"を"こんにちは"に置換する場合、以下のようにコードを書きます。

```Gleam
gleam/string.replace("hello world", "hello", "こんにちは")
```

これにより、"hello world"が"こんにちは world"に置き換わります。もし複数の置換を行いたい場合は、タプルを使用して複数の置換内容を指定することができます。

```Gleam
gleam/string.replace("hello world", [("hello", "こんにちは"), ("world", "世界")])
```

このように、Gleamでは簡単にテキストの検索と置換を行うことができます。

## テキストの検索と置換の詳細

Gleamでは、`gleam/string`モジュールの他にも様々な方法でテキストの検索と置換を行うことができます。例えば、`gleam/string/regex`モジュールを使用することで正規表現を使った検索と置換が可能になります。また、パターンマッチングや文字列操作のための便利な関数も提供されています。

さらに、文字列の検索と置換だけでなく、ファイルの中身の検索や置換も可能です。`gleam/fs`モジュールを使用することでファイルの読み込みや書き込みができ、その中で前述したテキストの検索と置換を行うことができます。

## 関連リンク

- [Gleamドキュメンテーション](https://gleam.run/documentation/)
- [Gleam GitHubリポジトリ](https://github.com/gleam-lang/gleam)