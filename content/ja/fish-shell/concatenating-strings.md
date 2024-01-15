---
title:                "文字列の連結"
html_title:           "Fish Shell: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

なぜ文字列を連結する必要があるのか？それは、プログラミングにおいて情報を生成したり、様々な処理を行う際に、文字列を組み合わせることが不可欠だからです。例えば、ユーザーにメッセージを表示する際には、そのメッセージを静的なものではなく、動的に変化させる必要があるでしょう。そのような場合に、文字列の連結を行うことで、より柔軟な情報の表現が可能になります。

## How To

`Fish Shell`では、文字列の連結に`string join`コマンドを使用することができます。まずは、以下のコードを試してみましょう。

```Fish Shell
set name "John"
set age 25
echo "My name is" (string join " " $name "and I am" $age "years old.")
```

上記のコードを実行すると、`My name is John and I am 25 years old.`というメッセージが出力されます。`string join`コマンドでは、第1引数に連結する際の区切り文字を指定し、後ろに連結したい文字列を指定することで、複数の文字列を簡単に連結することができます。

## Deep Dive

`string join`コマンドは、文字列を組み合わせる際に配列を使用することができます。例えば、以下のように配列を定義してみましょう。

```Fish Shell
set fruits ("apple" "orange" "banana")
```

この配列を利用して、区切り文字を指定せずに連結することも可能です。

```Fish Shell
echo (string join "" $fruits)
```

出力結果は、`appleorangebanana`となります。さらに、`string join`コマンドでは、配列に加えて標準入力からも文字列を取得することができるため、さまざまなオペレーションで応用することができます。

## See Also

- [Fish Shell documentation on String Operations](https://fishshell.com/docs/current/commands.html#string-operations)
- [Fish Shell tutorial on String Operations](https://www.digitalocean.com/community/tutorials/how-to-use-string-operations-in-fish-shell)