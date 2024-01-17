---
title:                "「テキストファイルの読み込み」"
html_title:           "PHP: 「テキストファイルの読み込み」"
simple_title:         "「テキストファイルの読み込み」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何か & なぜ？
テキストファイルを読み込むことは、プログラマーがよく行うタスクの一つです。これは、コンピューターに保存された文字列データを読み取り、処理することができるようにするためです。テキストファイルは、プログラム間でのデータの受け渡しや、設定ファイルの保存によく使用されます。

## 方法：
PHPを使用してテキストファイルを読み込む方法は簡単です。まずは`file`関数を使用して、読み込むテキストファイルのパスを指定します。この関数は、ファイルを配列として読み込みます。次に、`foreach`ループを使用して、行ごとに配列から読み取ったデータを処理します。

``` PHP
$file = file("sample.txt"); // テキストファイルのパスを指定する
foreach ($file as $line) {
	echo $line; // ファイルの内容を1行ずつ出力する
}

// 出力：
// This is a sample text file.
// It contains some important information.
// This file will be read by our program.
```

## 深堀り：
テキストファイルの読み込みには、PHPの`file`関数以外にも様々な方法があります。例えば、`fopen`関数を使用してファイルを開き、`fread`関数を使用してデータを読み込む方法もあります。また、テキストファイル以外にも、データベースやAPIからデータを読み込むこともできます。

テキストファイルの読み込みの歴史的背景としては、古くはテープやディスクなどの媒体からデータを読み込んでいました。現代では、インターネットやクラウドストレージからデータを読み込むことができるようになりました。

## 関連情報：
- PHP公式ドキュメント：https://www.php.net/manual/en/function.file.php
- `file`関数の使い方：https://www.w3schools.com/php/func_file_file.asp
- テキストファイルの読み書きについての基本的な知識：https://www.geeksforgeeks.org/working-with-text-files-in-php/