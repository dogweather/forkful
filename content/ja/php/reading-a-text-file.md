---
title:                "PHP: テキストファイルを読み込む"
simple_title:         "テキストファイルを読み込む"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むとは、PHPプログラミングの重要な部分です。ほとんどのプログラムでは、外部のデータを使用する必要があります。そのため、テキストファイルからデータを読み込み、それをプログラム内で使用できるようにすることは非常に有用です。

## 使い方

テキストファイルを読み込むには、まず`fopen()`関数を使用してファイルを開きます。それから、`fread()`関数を使用してファイルの内容を読み込みます。最後に、`fclose()`関数を使用してファイルを閉じます。

例えば、次のコードを使用すると、テキストファイルの内容を一行ずつ出力することができます。


```PHP
$file = fopen("textfile.txt", "r"); // ファイルを開く

// ファイルの内容を一行ずつ読み込む
while(!feof($file)) {
    echo fgets($file) . "<br>";
}

fclose($file); // ファイルを閉じる
```

上記のコードを実行すると、次のような出力が得られます。

```
Hello World!
This is a sample text file.
```

## ディープダイブ

テキストファイルを読み込む際には、いくつかの重要な点に注意する必要があります。

まず、ファイルパスを正しく指定する必要があります。また、ファイルの中身がどのような形式で保存されているかを知る必要があります。例えば、CSVファイルの場合は、`fgetcsv()`関数を使用することでデータを読み込むことができます。

また、テキストファイルを読み込む際には、エラー処理を行うことも重要です。例外処理を使用することで、エラーが発生した際にスクリプトが停止することを防ぐことができます。

## また読んでね

もしテキストファイルを書き込む方法も知りたい場合は、[PHPでテキストファイルを書き込む方法](https://example.com/how-to-write-to-text-file-in-php)をご覧ください。

また、テキストファイルを読み込むだけでなく、より高度なデータ操作を行いたい場合は、[PHPマニュアルのファイル処理](http://php.net/manual/ja/book.filesystem.php)を参考にすることをお勧めします。

## さらに知りたい場合

- [PHPでファイルを読み込む方法のドキュメント](http://php.net/manual/ja/function.fopen.php)
- [PHPでファイルを読み込む際に使用できるテストファイル型一覧](http://php.net/manual/ja/function.fopen.php#refsect1-function.fopen-parameters)
- [PHPでファイルを読み込む方法のサンプルコード](https://www.w3schools.com/php/php_file_open.asp)