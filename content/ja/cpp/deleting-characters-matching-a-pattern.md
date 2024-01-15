---
title:                "パターンにマッチする文字を削除する"
html_title:           "C++: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字列中から特定のパターンに合致する文字を削除することの意義を説明するため、プログラマーがこの作業に取り組む理由を最大限 2 つの文で説明します。

## How To

```C++
string str = "Hello 2020 World";
regex reg ("[0-9]+"); // 数字にマッチする正規表現パターン

string result = regex_replace(str, reg, ""); // パターンに合致する文字を削除する

cout << result; // "Hello World" という出力が得られる
```

## Deep Dive

文字列操作の中でも、特定のパターンに合致する文字を削除することは非常に便利です。例えば、テキスト中の数字やスペースを一括で削除したい場合に役立ちます。正規表現を使えば、パターンを柔軟に指定できるので、複雑な文字列の操作にも対応できます。

## See Also

- [C++ Regular Expressions](https://www.cplusplus.com/reference/regex/)
- [How to use Regex in C++](https://www.geeksforgeeks.org/regex-regular-expression-in-c/)