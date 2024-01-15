---
title:                "Japanese: パターンに一致する文字を削除する"
html_title:           "Bash: Japanese: パターンに一致する文字を削除する"
simple_title:         "Japanese: パターンに一致する文字を削除する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

日常的に、私たちはテキストファイルやプログラムファイルの中で不要な文字や単語を削除することがあります。特に、私たちは大量のファイルを扱うときや、あまり使われていないファイルを整理するときに、不要な文字をすばやく削除したいと思うことがあります。

## 方法

あるパターンに一致する文字を削除するには、Bashコマンドを使用して簡単に実行することができます。以下のコードブロックを参考にしてください。

```Bash
# 削除対象のパターンを指定します
pattern="example"

# 削除対象のファイルを指定します
file="example.txt"

# パターンに一致する文字を削除し、結果を新しいファイルに保存します
grep -v "${pattern}" ${file} > new_file.txt

# 元のファイルを削除し、新しいファイルの名前を元のファイルと同じにします
rm ${file}
mv new_file.txt ${file}

# 完了！指定したパターンに一致する文字がすべて削除されました。
```

## ディープダイブ

Bashコマンドを使用して文字を削除する方法はさまざまですが、一般的な方法としては`grep`コマンドを使用することができます。`grep -v`コマンドは、指定したパターンに一致しない文字列を出力することができ、そこで不要な文字を削除することができます。 また、`sed`コマンドを使用して、文字列を検索して置換することもできます。

## 関連リンク

- [Bash公式ドキュメンテーション](https://www.gnu.org/software/bash/)
- [Bashコマンドの基本](https://www.tutorialspoint.com/unix_commands/bash.htm)