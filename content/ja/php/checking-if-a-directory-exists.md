---
title:    "PHP: ディレクトリが存在するかどうかをチェックする。"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は何でしょうか？ディレクトリが存在するかどうかを確認することで、ファイルやデータが存在するかどうかを確認することができます。また、プログラムが期待通りに動作しているかを確認するためにも重要です。

## 方法

ディレクトリが存在するかどうかを確認するには、 ```file_exists()``` 関数を使用します。以下の例をご覧ください。

```PHP
<?php
$directory = 'example_directory';

if(file_exists($directory)){
    echo "ディレクトリが存在します。";
} else{
    echo "ディレクトリが存在しません。";
}
?>
```

この例では、まず ```$directory``` 変数に確認したいディレクトリの名前を指定します。その後、 ```file_exists()``` 関数を使用し、ディレクトリが存在するかどうかをチェックします。そして、条件分岐を使ってディレクトリが存在する場合は「ディレクトリが存在します。」と出力し、存在しない場合は「ディレクトリが存在しません。」と出力します。

## ディープダイブ

ディレクトリが存在するかどうかを確認する方法は他にもあります。例えば、 ```is_dir()``` 関数や ```scandir()``` 関数を使用する方法があります。また、ディレクトリが存在するかどうかだけでなく、パーミッションやパスの問題もチェックすることができます。さらに、ディレクトリの中身も取得して処理することができます。

## 参考リンク

- [PHP公式マニュアル - file_exists()関数](http://php.net/manual/ja/function.file-exists.php)
- [PHP公式マニュアル - is_dir()関数](http://php.net/manual/ja/function.is-dir.php)
- [PHP公式マニュアル - scandir()関数](http://php.net/manual/ja/function.scandir.php)