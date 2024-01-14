---
title:                "PHP: 一時ファイルの作成"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ作成するのか

一時的なファイルを作成する理由はたくさんあります。データを一時的に保存する必要がある、あるいはファイル操作を行う上で一時的なファイルが必要になる場合などがあります。今回は、PHPで一時的なファイルを作成する方法をご紹介します。

## 作成方法

まずは、どのようにして一時的なファイルを作成するかを見ていきましょう。PHPには `tempnam()` という関数が用意されており、これを使うことで一時的なファイルを作成することができます。以下のように書くと、一時的なファイルを作成してファイルパスを返してくれます。

```PHP
$temp_file = tempnam(sys_get_temp_dir(), 'php_');
echo $temp_file;
```

上記のコードを実行すると、例えば `/var/tmp/php_lno6H` のようなファイルパスが返ってきます。このファイルパスは一時的なファイルを作成するために利用されるものです。

次に、作成した一時的なファイルに何かしらのデータを書き込んでみましょう。以下のように書くと、一時的なファイルを作成して「Hello World!」というテキストを書き込んでくれます。

```PHP
$temp_file = tempnam(sys_get_temp_dir(), 'php_');
file_put_contents($temp_file, 'Hello World!');
echo file_get_contents($temp_file); //出力結果：Hello World!
```

一時的なファイルが作成できたかどうかを確認するためには、以下のようなコードを使うことができます。

```PHP
var_dump(file_exists($temp_file)); //出力結果：bool(true)
```

以上のように、 `tempnam()` を使うことで簡単に一時的なファイルを作成することができます。

## 深堀り

では、実際に`tempnam()` 関数の中身を見てみましょう。この関数はシステム上の一時ディレクトリと接頭辞を受け取って一時的なファイルを作成するものです。ファイル名は 接頭辞の後ろに `_` がついたものがファイル名となります。一時ディレクトリは `sys_get_temp_dir()` を使って取得することができます。

また、 `tempnam()` は単に一時的なファイルを作成するのに使うこともできますが、ファイル名を指定しなければ自動的に取得したファイル名が返されます。そのため、ファイルを一時的に作成するだけでなく、ファイル名を取得したい場合にも使うことができます。

## 他に参考になる情報

今回は `tempnam()` 関数を使った一時的なファイルの作成方法をご紹介しましたが、他にも一時的なファイルを扱う方法はたくさんあります。ぜひ以下のリンクを参考にしてみてください。

- [PHP: tempnam - Manual](https://www.php.net/manual/en/function.tempnam.php)
- [PHP: file_put_contents - Manual](https://www.php.net/manual/en/function.file-put-contents.php)

## その他

簡単ですが、一時的なファイルの作成方法についてご紹介しました。これから