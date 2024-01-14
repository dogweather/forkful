---
title:                "Bash: 「文字列を小文字に変換する」"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することに興味があるかもしれません。それは、文字列の大文字と小文字を区別せずに比較したり、特定の形式の入力を正規化するために役立つからです。

## 使い方

文字列を小文字に変換するには、`tr`コマンドを使用します。以下の例をご覧ください。

```Bash
# 変換前の文字列を変数に代入
str="HeLLo wOrLD"

# `tr`コマンドを使用して文字列を小文字に変換
lowercase_str=$(echo $str | tr '[:upper:]' '[:lower:]')

# 変換後の文字列を出力
echo $lowercase_str 

# 出力結果: hello world
```

## ディープダイブ

文字列を小文字に変換する方法はいくつかありますが、今回は`tr`コマンドに焦点を当てて説明します。このコマンドは、第一引数で指定した文字を第二引数で指定した文字に置き換えるという動作をします。つまり、`tr`コマンドを使用すると、大文字を小文字に置き換えることができます。

さらに、`tr`コマンドは正規表現を使用することもできます。例えば、以下のように記述することで、全ての文字を小文字に変換することができます。

```Bash
lowercase_str=$(echo $str | tr '[A-Z]' '[a-z]') 
```

また、`tr`コマンドはパイプを使用することで、他のコマンドの出力結果を処理することもできます。例えば、`ls`コマンドで取得したファイル名を全て小文字に置き換えてリストを表示することができます。

```Bash
ls | tr '[A-Z]' '[a-z]'
```

## 参考リンク

- [tr command in Bash](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
- [Using tr command](https://linuxize.com/post/linux-tr-command/)
- [Bash scripting tutorial – A second programming language](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [10 Bash tips you should know for your job as a sysadmin](https://opensource.com/article/18/5/bash-tricks)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)

## 参考文献

1. [tr command in Bash](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
2. [Using tr command](https://linuxize.com/post/linux-tr-command/)
3. [Bash scripting tutorial – A second programming language](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
4. [10 Bash tips you should know for your job as a sysadmin](https://opensource.com/article/18/5/bash-tricks)
5. [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)