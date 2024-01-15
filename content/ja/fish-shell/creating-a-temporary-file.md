---
title:                "一時ファイルの作成"
html_title:           "Fish Shell: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は様々です。プログラミング中に一時的にデータを保存する必要がある場合や、プログラム実行中にファイルを生成する必要がある場合に便利です。

## 使い方

```Fish Shell
set tmpfile (mktemp)
echo "Hello World" > $tmpfile # $tmpfileにテキストを書き込む
cat $tmpfile  # テキストが正しく書き込まれたか確認する
rm $tmpfile # プログラムが終了したら、一時ファイルを削除する
```

`mktemp`コマンドを使用することで、一時的なファイルを作成することができます。`$`を使用することで変数を定義し、一時ファイルの名前を指定しています。その後、必要な処理を行い、プログラムが終了したら一時ファイルを削除することで、メモリの使用量を減らすことができます。

## 詳細を追う

一時ファイルの作成方法は簡単ですが、実際の処理はどうなっているのでしょうか。まず、`mktemp`コマンドを実行すると、一時ファイルを作成するための一意なファイル名が返されます。これを変数に格納することで、後の処理で使用することができます。

次に、`echo`コマンドを使用して、文字列を一時ファイルに書き込みます。これにより、ファイルが実際に作成されます。そして、`cat`コマンドを使用して、書き込んだ文字列が正しくファイルに保存されているか確認します。

最後に、`rm`コマンドを使用して、一時ファイルを削除します。プログラムが終了したら、使用した一時ファイルを削除することで、不要なファイルが残らずメモリの使用量を減らすことができます。

## 参考リンク

* [Fish Shell Documentation](https://fishshell.com/docs/current/)
* [How-To Geek: What Is a Temporary File?](https://www.howtogeek.com/104068/htg-explains-what-is-a-temporary-file-is-it-safe-to-delete-them/)