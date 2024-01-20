---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字を削除するとは、特定のパターンに一致する文字列を削除することを意味します。これは、ファイルから不必要な文字やコードを取り除くためにプログラマがしばしば行います。

## 実施方法：

Bashでは`tr`コマンドを使ってパターンに一致する文字を削除できます。
```Bash
echo "Hello, World!" | tr -d '!'
```
出力：
```Bash
Hello, World
```
上記の例では、"!"符号を削除しています。

## ディープダイブ：
 
`tr`コマンドは、Unix系OSで初めて登場しました。そのため、パターンマッチングで文字を削除するのに最適な選択肢の一つだと言えます。しかし、`sed`や`awk`などの他のツールも同じ任務を達成することが可能です。

この削除操作が実際に何を行っているかたどると、Bashは入力文字列を走査し、削除すべき文字を見つけた場合に、その文字を結果から除外します。これはまた、Bashがテキストストリームを操作する非常に強力な方法の一つでもあります。

## 参考資料:

- [Bash scriptingの基本的なトピック](https://developer.ibm.com/jp/languages/bash/articles/ja-bash/)
- [`tr`コマンドについての詳細解説](https://www.thegeekstuff.com/2012/12/linux-tr-command/)
- [`sed`と`awk`の使用方法](https://www.tutorialspoint.com/unix/unix-regular-expressions.htm)