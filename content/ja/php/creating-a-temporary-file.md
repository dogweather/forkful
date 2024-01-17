---
title:                "一時ファイルを作成する"
html_title:           "PHP: 一時ファイルを作成する"
simple_title:         "一時ファイルを作成する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
一時ファイル（temporary file）とは、一時的にデータを保存し、後で削除されるファイルのことです。プログラマーは、一時ファイルを作成することで、データの永続的な保存を防ぎ、データの一時的な利用を可能にします。

## 方法：
```PHP
// PHPで一時ファイルを作成する方法はいくつかあります。

// 1. tmpfile()関数を使用して一時ファイルを作成します。
// この関数は、一時ファイルのハンドルを返し、ファイルの読み書きに使用できます。
$tempFile = tmpfile();

// 2. fopen()関数を使用して一時ファイルを作成します。
// 第二引数に "w+" を指定することで、読み書き可能な一時ファイルが作成されます。
$tempFile = fopen('tempfile.txt', 'w+');

// 一時ファイルに書き込みを行います。
fwrite($tempFile, 'Hello World!');

// 作成したファイルは自動的に削除されるため、明示的に削除する必要はありません。

// ファイルを閉じます。
fclose($tempFile);

// 作成したファイルがあるかどうかを確認することもできます。
if (file_exists('tempfile.txt')) {
    echo '一時ファイルが作成されました。';
} else {
    echo '一時ファイルが作成されませんでした。';
}
```

## 詳細を調べる：
一時ファイルの概念は、プログラミングの世界において古くから存在しています。一時的なデータの利用が増えるにつれて、一時ファイルの需要も高まりました。

一時ファイルの代替手段として、データベースやセッションストレージを使用することができます。しかし、一時ファイルは軽量であるため、大量の一時データを扱う場合は依然として有用です。

PHPでは、一時ファイルがデフォルトで/tmpディレクトリに作成されますが、php.iniファイルのsys_temp_dirディレクティブを変更することで、一時ファイルの保存先を変更することもできます。

## 関連情報を参照：
- [PHP公式ドキュメント - tmpfile()](https://www.php.net/manual/ja/function.tmpfile.php)
- [PHP公式ドキュメント - fopen()](https://www.php.net/manual/ja/function.fopen.php)
- [PHP公式ドキュメント - sys_temp_dir](https://www.php.net/manual/ja/ini.core.php#ini.sys-temp-dir)