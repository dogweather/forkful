---
title:    "Bash: 文字列の連結"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結することの意義は、しばしばプログラムを作成する上で見過ごされがちです。しかし、実際には文字列の連結は、データの表現や処理の効率性に大きな影響を与えることがあります。例えば、複数の変数の値を組み合わせてファイルのパスを作成する場合、文字列の連結を行うことで簡単に実現できるようになります。

## 方法

Bashプログラムでは、`echo`コマンドを使用して文字列を出力することができます。しかし、複数の文字列を結合する場合はどのようにすれば良いのでしょうか？ここでは、`echo`コマンドとバックスラッシュ`\`を使用して、文字列を連結する方法を示します。

```Bash
echo "Hello, " \
"world!"
```

このコマンドでは、最初の`echo`は単語"Hello,"を出力し、バックスラッシュの次の行のコマンドである"world!"は続く行にテキストとして表示されます。このように、バックスラッシュを使用することで複数の行に分けて文字列を連結することができます。

## 深堀り

文字列を連結するには他にも方法があります。例えば、ダブルクォーテーション`"`を使用して複数の変数を連結する方法です。

```Bash
name="John"
greeting="Hello, $name!"
echo $greeting
```

実行すると、"Hello, John!"という出力が得られます。このように、変数を使用しても文字列を連結することができます。

また、単語連結演算子`+`を使用する方法もあります。以下の例では、3つの変数を連結してファイル名を作成しています。

```Bash
year="2022"
month="_05"
day="_20"
file="report"($year+$month+$day)
echo $file
```

実行すると、"report2022_05_20"という出力が得られます。

## 参考リンク

- [Bash scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [コマンドラインでBashを使用する方法](https://ja.wikipedia.org/wiki/Bash)
- [よく使用されるBashコマンドのリスト](https://www.activestate.com/blog/basic-bash-commands/)
- [文字列の比較と操作について](https://ubuntu.com/tutorials/command-line-for-beginners#5-1-string-comparison-and-manipulation)

## 関連リンク

- [文字列を連結する方法について知る](https://linuxhint.com/bash_string_concatenation/)
- [変数を使用して文字列を作成する方法](https://unix.stackexchange.com/questions/15136/using-a-variable-to-a-file-name-in-bash)