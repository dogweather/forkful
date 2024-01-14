---
title:                "Bash: テキストの検索と置換"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

なぜテキストの検索と置換に取り組むのか、その理由をご紹介します。テキストの検索と置換は、プログラミングにおいて非常に便利な機能です。例えば、大量のファイルの中から特定の単語やフレーズを検索し、一括置換することができます。

## 方法

テキストの検索と置換をBashで行う方法をご紹介します。まず、必要なコマンドは「sed」です。次のコマンドを使用して、ファイル内の特定の文字列を別の文字列に置換することができます。

```Bash
sed 's/置換前の文字列/置換後の文字列/g' ファイル名
```

例えば、ファイル内の「cat」を「dog」に置換する場合は、次のようにコマンドを入力します。

```Bash
sed 's/cat/dog/g' sample.txt
```

すると、ファイル内のすべての「cat」が「dog」に置換されます。また、ファイル全体ではなく特定の行のみを置換することもできます。その場合は、行番号を指定する必要があります。

```Bash
sed '行番号s/cat/dog/g' sample.txt
```

## ディープダイブ

さらに詳しく説明すると、sedコマンドでは文字列の置換の他にもさまざまな操作ができます。例えば、行の削除や文字列の追加などが可能です。また、正規表現を使用して複数の文字列を一括置換することもできます。詳しい説明はマニュアルを参照してください。

## その他の参考リンク

- マニュアル: https://linuxcommando.blogspot.com/2007/10/find-and-replace-text-within-file-using.html
- 正規表現について: https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/
- 実践的な使用例: http://www.theunixschool.com/2012/07/sed-examples-replace-remove-preserve.html

## 参考リンク