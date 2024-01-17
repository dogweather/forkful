---
title:                "文字列を小文字に変換する"
html_title:           "Bash: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何？なぜ？
文字列を小文字に変換するとは、コンピューターで処理されるプログラム内の文字列を全て小文字に変えることを指します。プログラマーは、文字列を小文字に変換することで、文字列を比較する際に大文字と小文字の区別をなくすことができます。

## 方法：
```Bash
# 文字列を小文字に変換する例
my_string="HeLLo WORld"
my_string_lower="${my_string,,}"
echo $my_string_lower
# 出力結果：hello world
```

```Bash
# 環境変数を小文字に変換する例
my_env_var="HELLO=World"
my_env_var_lower="${my_env_var,,}"
echo $my_env_var_lower
# 出力結果：hello=world
```

```Bash
# 文字列を大文字に変換する例
my_string="heLLo WORld"
my_string_upper="${my_string^^}"
echo $my_string_upper
# 出力結果：HELLO WORLD
```

## 深堀り：
文字列を小文字に変換する機能は、過去のコンピューター言語においてはなかったものです。代わりに、文字列をすべて大文字に変換する機能が利用されていました。しかし、近年では多くのプログラミング言語が文字列を小文字に変換する機能をサポートしています。また、大文字と小文字の区別を無くすために、正規表現などの他の方法も利用されています。

## 関連リンク：
- [Bash man page](https://www.gnu.org/software/bash/manual/html_node/String-Expansion.html)
- [正規表現を用いた文字列の置換](https://qiita.com/ryo3808/items/17bb232303c62506558e)