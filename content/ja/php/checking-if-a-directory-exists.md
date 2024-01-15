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

## Why

ディレクトリが存在しているかどうかは、ファイルの読み書きやアクセス制御など、多くのプログラミングタスクで重要な役割を果たします。そのため、PHPではディレクトリの存在をチェックする方法が用意されています。

ファイルシステム内で特定のディレクトリが存在するかどうかを確認することで、ユーザーに適切なアクセスを提供することができ、セキュリティを強化することができます。

## How To

PHPでは、`file_exists()`関数を使用してディレクトリの存在を確認することができます。以下のコードを使用することで、指定したディレクトリの存在を確認することができます。

```PHP
<?php
$directory = '/path/to/directory';

if (file_exists($directory)) {
  echo "The directory exists!";
} else {
  echo "The directory does not exist.";
}
?>
```

上記の例では、指定したディレクトリが存在する場合には「The directory exists!」、存在しない場合には「The directory does not exist.」というメッセージが表示されます。

## Deep Dive

`file_exists()`関数は、指定されたパスがファイルでもディレクトリでも存在する場合には`true`を返し、存在しない場合には`false`を返します。そのため、ディレクトリの存在をチェックする際には、ファイルの存在も同時にチェックすることになります。

また、PHPでは`is_dir()`関数を使用してもディレクトリの存在を確認することができます。`is_dir()`関数は、指定されたパスがディレクトリである場合には`true`を返し、そうでない場合には`false`を返します。そのため、ファイルの存在をチェックする必要がない場合には、`is_dir()`関数を使用する方法もあります。

## See Also

- PHP公式ドキュメント: [file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- PHP公式ドキュメント: [is_dir()](https://www.php.net/manual/en/function.is-dir.php)