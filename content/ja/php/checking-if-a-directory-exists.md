---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "C#: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認するとは、指定したディレクトリが実際に存在するかどうかを調べることです。これは誤ったパスを使用してエラーを回避するためや、特定のディレクトリに対する処理を行う前にそのディレクトリが存在することを確認するために必要です。

## 方法:

PHPでディレクトリの存在をチェックする基本的な方法は以下の通りです。

```PHP
if(is_dir($directory_path)){
    echo "Directory exists";
} else {
    echo "Directory does not exist";
}
```

このスクリプトは '$directory_path' 変数に保存されたディレクトリが存在するかどうかをチェックします。存在する場合、"Directory exists"と表示します。存在しない場合は、"Directory does not exist"と表示します。

## 深く見る

以前のPHPバージョンでは、ディレクトリの確認方法は少し異なりました。PHP 4.xでは、'is_dir()'関数はまだ実装されておらず、「file_exists」関数を使用してディレクトリの存在をチェックしていました。

しかし、この方法は完全ではありません。「file_exists()」関数は、ファイルまたはディレクトリ名が存在するかどうかをチェックしますが、それがファイルかディレクトリかは区別しません。そのため、名前が一致するファイルが存在する場合でも、ディレクトリが存在しなくても「true」を返します。

別のオプションとして、'scandir()'関数を使用してディレクトリの存在を確認することもできます。ただし、これは内容をリストするためのもので、ディレクトリが大きい場合にはパフォーマンスの問題が生じる可能性があります。

## 参考資料:

更に詳細な情報については以下のリンクを参照してください。

1. PHP公式ドキュメンテーション: [PHP: is_dir - Manual](https://www.php.net/manual/en/function.is-dir.php)
2. PHP公式ドキュメンテーション: [PHP: file_exists - Manual](https://www.php.net/manual/en/function.file-exists.php)
3. PHP公式ドキュメンテーション: [PHP: scandir - Manual](https://www.php.net/manual/en/function.scandir.php)