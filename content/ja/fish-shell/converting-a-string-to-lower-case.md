---
title:                "Fish Shell: 「文字列を小文字に変換する」"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ？

文字列を小文字に変換することの利点は何でしょうか？一般的に、文字列を小文字に変換することはプログラムの標準形式にすることができるため、データの整形や検索などの処理をしやすくなります。

## 使い方

```Fish Shell
set string "Hi, My Name is John."  
echo $string | tr '[:upper:]' '[:lower:]'
```

出力： hi, my name is john.

`tr`コマンドは、テキストの変換を行うコマンドです。引数に`[:upper:]`と`[:lower:]`を指定することで、大文字を小文字に変換することができます。このコマンドを利用することで、簡単に文字列を小文字に変換することができます。

## 深堀り

文字列を小文字に変換する方法は、プログラミング言語によって異なりますが、Fish Shellでは`tr`コマンドを使うことができます。これはUNIX系のオペレーティングシステムでよく使われるコマンドで、実際には文字の置換を行うコマンドです。`tr`コマンドの詳細については、[公式ドキュメント](https://fishshell.com/docs/current/cmds/tr.html)を参照してください。

さらに、Fish Shellでは`string`コマンドを使うことでも文字列を小文字に変換することができます。こちらは、文字列を変数として定義し、`string`コマンドを利用して変換を行います。詳細な使い方については、[公式ドキュメント](https://fishshell.com/docs/current/cmds/string.html)を参照してください。

## 併せて読みたい

- [Fish Shell Quickstart](https://fishshell.com/docs/current/tutorial.html)
- [UNIXコマンドの基本](https://qiita.com/take-yan/items/5b9a91c5fca9dce210bb)
- [文字列の置換をするコマンドの使い方](https://qiita.com/take-yan/items/09bed116928c42775ca7)

より詳細な内容については、上記リンクを参照してください。

## 参考文献

- [公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [Tutorial for Unix Commands](http://www.electronics.dit.ie/staff/tscarff/using_unix_commands.html)