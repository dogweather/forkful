---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Bash: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何かしら確認したかったら?

ディレクトリが存在するかどうかの確認とは、プログラマーがコマンドを使用して、特定のディレクトリが存在するかどうかを調べることです。これを行う理由は、プログラムの実行中にバグが発生しないようにするためです。

## 方法：

```bash
if [ -d "directory_name" ]; then
  echo "ディレクトリは存在します。"
fi
```

これは、特定のディレクトリが存在するかどうかを確認するための基本的な方法です。 ```directory_name```の部分は、チェックしたいディレクトリの名前に変更する必要があります。

```bash
if [ -d "/home/user/directory_name" ]; then
  echo "ディレクトリは存在します。"
fi
```

このコードでは、絶対パスを使用して特定のディレクトリをチェックしています。

## 深堀り：

### 歴史的文脈：

ディレクトリが存在するかどうかを確認するためのコマンドの使用は、UNIXの初期にさかのぼります。その後、Bashシェルで広く使用されるようになりました。

### 代替方法：

Bashシェル以外にも、PerlやPythonなどのスクリプト言語を使用してディレクトリの存在を確認することもできます。また、Bashの他のコマンドやツールを使用することで、より高度な方法でディレクトリの存在を確認することも可能です。

### 実装の詳細：

ディレクトリの存在を確認する際に使用されるコマンドは、実際にはシェルの組み込みコマンドではありません。それは代わりに、シェルのコマンドのリストから見つける必要があります。
これを行うために、通常は ```type -a```コマンドを使用します。

例えば、```type -a [```コマンドを実行すると、 ```[```コマンドのバリアントをリストで返すことができます。この中に、```test```コマンドを見つけることができ、そのバリアントとしてディレクトリの存在をチェックする機能が含まれています。

## 関連情報:

- [Bash Guide for Beginners - Working with directories](http://www.rayninfo.co.uk/tips/bash.htm#check_if_directory_exists)
- [GNU Bash Manual - Conditional Constructs](https://www.gnu.org/software/bash/manual/html_node/Conditional-Constructs.html)