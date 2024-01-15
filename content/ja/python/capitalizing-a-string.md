---
title:                "文字列のキャピタライズ"
html_title:           "Python: 文字列のキャピタライズ"
simple_title:         "文字列のキャピタライズ"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

文字列の最初の文字を大文字に変換することで、見栄えを良くしたり、データを整形したりすることができます。

## How To

```Python
# 文字列を大文字に変換する方法
my_string = "hello world"
capitalized_string = my_string.capitalize()

print(capitalized_string)

# 出力結果：Hello world
```

```Python
# 文字列の最初の文字のみを大文字に変換する方法
my_string = "hello world"
capitalized_string = my_string.title()

print(capitalized_string)

# 出力結果：Hello World
```

## Deep Dive

大文字に変換する方法は、文字列を操作する際に非常に便利です。Pythonでは、`capitalize()`や`title()`などの組み込み関数を使用することで、簡単に文字列を大文字に変換することができます。また、正規表現を使うことで、特定のパターンにマッチする文字列の最初の文字を大文字に変換することも可能です。

## See Also

- [Python 文字列](https://docs.python.org/ja/3/library/string.html)
- [正規表現入門](https://docs.python.org/ja/3/howto/regex.html)