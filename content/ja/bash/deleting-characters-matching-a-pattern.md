---
title:                "パターンに一致する文字を削除する"
html_title:           "Bash: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何？なんで？
パターンにマッチする文字を削除することは、プログラマーがコードをより効率的にするために使用する方法です。この方法を使うことで、不要な文字や文字列を簡単に削除することができます。

## 方法：
Bashには、パターンにマッチする文字を削除するコマンドが用意されています。例えば、以下のように使用します。

```Bash
#文字列に "abc" が含まれている場合に削除する
echo "abcedf" | sed 's/abc//g'
```
このコマンドを実行すると、"abcedf" という文字列から "abc" が削除され、結果として "edf" が出力されます。

## 深層掘り下げ：
この方法は、実際には非常に古い時代から使われてきた方法です。もともとは文字列を検索して置換するためのテキスト処理ツールとして開発されましたが、今ではプログラミング言語やコマンドラインでよく利用されています。代替方法としては、パターンマッチングや正規表現などが挙げられますが、Bashのコマンドを使うことで簡潔に文字列の削除を行うことができます。また、実装の詳細としては、Bashの内部的にはsedというコマンドを使用していることが知られています。

## みてね：
- [sedコマンドの公式ドキュメント](https://www.gnu.org/software/sed/manual/sed.html)
- [Bashの公式ドキュメント](https://www.gnu.org/software/bash/manual/bash.html)