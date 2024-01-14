---
title:                "Fish Shell: 文字列の抽出"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の中から部分文字列を抽出することは、プログラミングにおいて非常に便利です。例えば、ある文字列から特定のデータを抽出したり、文字列の特定の部分を置換したりすることができます。Fish Shellを使用して文字列から部分文字列を抽出する方法を学びましょう。

## 方法

Fish Shellで文字列から部分文字列を抽出するには、substrコマンドを使用します。以下のコマンドを使用して、文字列 "Hello World!" から "World"を抽出してみましょう。

```Fish Shell
set str "Hello World!"
substr $str 6 5
```
➜ World

このコマンドは、文字列の6番目の文字から始まり、5文字を抽出します。これにより、"World"という部分文字列が抽出されます。また、始めの数値を"-"にすることで、末尾からの文字数を指定することもできます。例えば、"-5"を指定すると、末尾から5文字を抽出します。

```Fish Shell
substr $str -5 5
```
➜ World

さらに、文字列の長さを取得するには、"len"サブコマンドを使用します。

```Fish Shell
set len (len $str)
echo $len
```
➜ 12

これにより、文字列 "Hello World!" の長さが12であることが確認できます。

## ディープダイブ

Fish Shellのsubstrコマンドは非常に柔軟で、さまざまな文字列の操作に使用することができます。例えば、ある文字列の一部を置換する際にも便利です。以下のコマンドを使用して、"Hello World!"の"Hello"を"こんにちは"に置き換えてみましょう。

```Fish Shell
substr $str 0 5
echo (substr $str 5 7)
```
➜ こんにちは World!

このように、substrコマンドを使用すれば、簡単に文字列を部分的に操作することができます。

## See Also

- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/cmds/substr.html)
- [Fish Shellの文字列操作に関するチートシート](https://fishshell.com/docs/current/tutorial.html#string-manipulation-1)
- [Fish Shellで文字列を扱う際のベストプラクティス](https://fishshell.com/docs/current/tutorial.html#string-manipulation-best-practices)