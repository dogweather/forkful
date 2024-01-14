---
title:                "PHP: 「ディレクトリが存在するかどうかを確認する」"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認するのは、プログラミングにおいて非常に重要なタスクです。データのストレージやファイル管理など、様々な理由でディレクトリの存在をチェックする必要があります。

## 方法

PHPでは、`is_dir()`関数を使用してディレクトリが存在するかどうかを確認することができます。以下は例です。

```PHP
$directory = "/path/to/directory";

if (is_dir($directory)) {
    echo "ディレクトリは存在します。";
} else {
    echo "ディレクトリは存在しません。";
}
```

上記のコードを実行すると、指定したディレクトリが存在するかどうかに応じてメッセージが表示されます。

## ディープダイブ

ディレクトリが存在するかどうかを確認する方法は、実際には非常にシンプルです。しかし、ディレクトリサイズやファイルパーミッションなど、より詳細な情報を知りたい場合は、`file_exists()`関数を使用することで可能になります。また、`glob()`関数を使うことで、ディレクトリ内のファイルやサブディレクトリを取得することもできます。このように、`is_dir()`関数はディレクトリが存在するかどうかを確認するだけではなく、さまざまな応用が可能な関数であることがわかります。

## 参考資料

- [PHP マニュアル - is_dir()](https://www.php.net/manual/ja/function.is-dir.php)
- [PHP マニュアル - file_exists()](https://www.php.net/manual/ja/function.file-exists.php)
- [PHP マニュアル - glob()](https://www.php.net/manual/ja/function.glob.php)
- [Qiita - PHPでディレクトリが存在するかどうかを確認する方法](https://qiita.com/sutara79/items/6ae3b629b52f6defef40)