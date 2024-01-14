---
title:    "Bash: 文字列を大文字にする"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## なぜ 
私たちが文字列の大文字化に取り組む理由は、特定の言語や処理系で大文字と小文字が区別されるためです。より正確に、文字列の大文字化は、大文字と小文字を混同するリスクを避けるために、プログラム内にある特定の文字列をすべて大文字にする必要があるときに頻繁に使用されます。

## 使い方 
文字列を大文字にする方法は、Bashプログラミング言語を使用したコード例を使用して説明します。

```Bash
# 元の文字列を変数に保存 
str="hello world"

# 文字列を大文字に変換して出力 
echo "${str^^}"

## 出力結果: HELLO WORLD
```

このコードでは、大文字に変換したい文字列を変数に保存し、変数名の後に`^^`を追加することで、文字列を大文字に変換することができます。

## ドミドールブ 
文字列を大文字に変換する方法については、`tr`コマンドを使用することもできます。例えば、次のコードは`tr`コマンドを使用して文字列を大文字に変換する例です。

```Bash
# 元の文字列を変数に保存 
str="hello world"

# 大文字に変換して出力 
echo "$str" | tr '[:lower:]' '[:upper:]'

## 出力結果: HELLO WORLD
```

また、このコマンドでは、大文字に変換したい文字列を変数に保存せずに直接指定することもできます。

```Bash
# 大文字に変換して出力 
echo "hello world" | tr '[:lower:]' '[:upper:]'

## 出力結果: HELLO WORLD
```

さらに、`sed`コマンドを使用することで、特定の文字列だけを大文字に変換することもできます。以下のコードは、`sed`コマンドを使用して両方の単語の最初の文字を大文字に変換する例です。

```Bash
# 元の文字列を変数に保存 
str="hello world"

# 最初の文字を大文字に変換して出力 
echo "$str" | sed -e 's/\b\(.\)/\U\1/g'

## 出力結果: Hello World
```

## 参考リンク 
- [Bash Guide for Beginners: Commands for Manipulating Strings](https://bash.cyberciti.biz/guide/String_manipulation_in_Bash)
- [Introduction to Bash Scripting](https://devhints.io/bash)
- [Unix Tutorial: Learn Bash in 10 Minutes](https://www.unixtutorial.org/learn-bash-in-10-minutes) 

## 参照 
- Markdownシンタックスは[こちら](https://www.markdownguide.org/basic-syntax/)をご覧ください。