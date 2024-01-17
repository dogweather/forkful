---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "PHP: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
最新のPHPでは、ディレクトリが存在するかどうかをチェックすることができます。プログラマーがこのチェックを行う理由は、ファイルやディレクトリが存在しない場合に処理を適切に行うためです。

## 方法：
```PHP
<?php
// ディレクトリが存在するかをチェック
if (is_dir('directory_name')) {
    echo '存在します';
} else {
    echo '存在しません';
}
?>
```
`directory_name` の部分にチェックしたいディレクトリの名前を入れます。ディレクトリが存在する場合は「存在します」と表示され、存在しない場合は「存在しません」と表示されます。

## 深堀り：
この機能は、PHP 4.0.3以降で利用可能です。ディレクトリが存在するかどうかをチェックするもう一つの方法は、`file_exist()`関数を使用することです。しかし、`is_dir()`関数の方がより効率的であり、ライブラリファイルの読み込みといった追加の処理が必要ありません。また、`is_dir()` 関数は、ディレクトリ以外のファイルを渡されても正しくチェックすることができます。

## 関連リンク：
- [PHP is_dir() 関数のドキュメント（英語）](https://www.php.net/manual/en/function.is-dir.php)
- [PHP file_exist() 関数のドキュメント（英語）](https://www.php.net/manual/en/function.file-exists.php)
- [PHPのis_dir()関数の使い方（日本語）](https://uxmilk.jp/17587)