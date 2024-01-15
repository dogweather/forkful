---
title:                "文字列の大文字化"
html_title:           "Fish Shell: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の大文字化を行う理由を最大2文で説明すると、人々がより読みやすい形式でデータを表示したり、エラーを防ぐために文字列を大文字化する必要があるからです。

## 方法

Fish Shellを使用して文字列を大文字化するには、まず`string`という変数に大文字化したい文字列を格納します。

```
fish shell
string="hello world"
```

次に、以下のようなコマンドを使用して`string`を大文字化できます。

```
fish shell
set string (string | tr '[a-z]' '[A-Z]')
```

このコマンドでは、パイプを使用して`string`を`tr`コマンドに渡し、文字を小文字から大文字に変換させています。

大文字化された`string`を出力するには、以下のコマンドを使用します。

```
fish shell
echo $string
```

これにより、`HELLO WORLD`という出力結果を得ることができます。

## 深堀り

文字列を大文字化する際、注意すべき点があります。それは、国や地域によって大文字化のルールが異なる可能性があることです。例えば、ドイツ語では`ß`を`SS`とグローバルに置き換えることが一般的ですが、他の言語では行われないかもしれません。そのため、文字列を大文字化する際は、その言語や国のルールに従って適切に変換する必要があります。

## 参考サイト

- [Official Fish Documentation](https://fishshell.com/docs/current/)
- [Tr Command Documentation](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [String Manipulation in Fish Shell](https://medium.com/@onlyshk/fish-shell-tutorial-part-3-string-manipulation-in-fish-c3b4775fcc2)