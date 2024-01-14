---
title:                "Bash: 「パターンにマッチする文字を削除する」"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

特定のパターンと一致する文字を削除するのに、人々がなぜ関わるかを説明するわずか1-2文。

パターンマッチングはBashプログラミングでよく使われる技術です。例えば、複数のファイル名から特定の拡張子を持つファイルを削除する際に、パターンマッチングを使用することができます。これにより、手作業でファイルを1つずつ選択する必要がなく、簡単かつ迅速に削除できます。

## 方法

パターンにマッチする文字を削除するベストな方法は、`sed`コマンドを使用することです。以下のような構文を使用します。

```
sed 's/パターン//g' input_file
```

このコマンドは、`input_file`内のすべての行からパターンにマッチする文字を削除します。 `s`はsubstitute（置換）を意味し、`g`はglobal（すべて）を意味します。つまり、行内のすべてのパターンにマッチする文字が削除されます。

例として、以下のようなテストファイルを作成しましょう。

```
test1.txt
test2.html
test3.sh
test4.txt
```

`txt`拡張子を持つファイルを削除するために、`sed`コマンドを使用しましょう。

```
sed 's/txt//g' test_file
```

これにより、`test1.txt`と`test4.txt`が削除され、以下の結果になります。

```
test2.html
test3.sh
```

## 深堀り

パターンマッチングを使用することで、より複雑なパターンにも対応することができます。例えば、ファイル名の一部ではなく、特定の文字パターンにマッチするファイルを削除することも可能です。

また、`sed`コマンドのオプションを使用することで、マッチした文字列の置換を行うことも可能です。例えば、`test1`という文字列を`new_test`に置換する場合は、以下のようにオプションを追加します。

```
sed 's/test1/new_test/g' test_file
```

これにより、`test1.txt`が`new_test.txt`に置換されます。

## 参考リンク

- [Bashパターンマッチングチュートリアル](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
- [sedコマンドのドキュメント](https://www.gnu.org/software/sed/manual/sed.html)
- [スラドのsed使いこなしTipsまとめ](https://linux.srad.jp/story/11/09/04/0434206/)