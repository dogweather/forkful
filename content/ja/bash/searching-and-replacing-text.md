---
title:                "テキストの検索と置換"
html_title:           "Bash: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換をすることの利点は、大量のテキストを簡単に変更し、特定のパターンに基づいた複数の変更を一度に行うことができることです。例えば、同じ文言を使用する複数のファイルを一度に変更したい場合などに役立ちます。

## 方法

まず、置換を行いたいファイルを開きます。次に、以下のコマンドを使用して、テキストを検索して置換します。

```
Bash
sed -i 's/検索するテキスト/置換するテキスト/g' ファイル名
```

このコマンドは、ファイル内の全てのインスタンスで検索したテキストを置換することができます。`ファイル名`の部分には、置換を行うファイルの名前を指定します。

例えば、`sample.txt`というファイルに出てくる`apple`を`orange`に置換する場合は、以下のようにコマンドを入力します。

```
Bash
sed -i 's/apple/orange/g' sample.txt
```

これにより、`sample.txt`内の全ての`apple`が`orange`に置換されます。

## ディープダイブ

上記の例では、単純な置換の方法を紹介しましたが、プレースホルダーや正規表現を使用することで、より複雑な検索と置換を行うこともできます。

例えば、ファイル内のすべての数字を増やしたい場合、以下のようにコマンドを入力します。

```
Bash
sed -i 's/[0-9]/＆ + 1/g' sample.txt
```

これにより、ファイル内のすべての数字が1つずつ増えます。

また、プレースホルダーと正規表現を組み合わせることで特定のパターンに基づいた置換を行うこともできます。例えば、ファイル内の`hello1`という文字列を全て`hello2`に置換する場合は、以下のようにコマンドを入力します。

```
Bash
sed -i 's/hello[0-9]/＆2/g' sample.txt
```

これにより、`sample.txt`内の全ての`hello`に続く数字が2に置換されます。つまり、`hello1`は`hello2`に、`hello10`は`hello20`に置換されます。

## See Also

- [Bash Shell Scripting Basics](https://www.taniarascia.com/how-to-create-and-use-bash-scripts/)
- [Sed - An Introduction and Tutorial](https://www.grymoire.com/Unix/Sed.html)
- [Regular Expressions Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)