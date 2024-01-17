---
title:                "テキストファイルの作成"
html_title:           "PHP: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# What & Why?
テキストファイルを書くとは何か？プログラマーがそれをする理由は何か？

テキストファイルとは、テキスト形式で情報を保存することができるファイルのことです。プログラマーは、プログラミング言語でテキストファイルを作成することで、プログラムに必要なデータを保存および読み込むことができます。

# How to:
テキストファイルを作成するには、PHPの```fopen()```関数を使用します。 以下の例は、"text.txt"という名前のテキストファイルを作成し、その中に"Hello World!"というテキストを書き込んでいます。

```PHP
$file = fopen("text.txt", "w");
fwrite($file, "Hello World!");
fclose($file);
```

実行すると、"text.txt"ファイルが作成され、その中に"Hello World!"というテキストが書き込まれます。

# Deep Dive:
歴史的な文脈や代替手段、テキストファイルを作成する方法についてのさらなる情報。

テキストファイルは、コンピューターの登場以前から存在していました。 しかし、今でもテキストファイルは、さまざまな言語やプログラムでデータを保存するために一般的に使用されています。代替手段としては、データベースやスプレッドシートなどのデータベース管理システムがありますが、小さなデータセットを扱う場合や、実行速度が重要な場合は、テキストファイルの使用が推奨されます。

テキストファイルを作成する方法には、他にも```file_put_contents()```や```fwrite()```以外の方法がありますが、基本的には同じような手順でテキストファイルを作成することができます。

# See Also:
関連する情報源へのリンク。

- PHP公式ドキュメント - fopen関数：https://www.php.net/manual/en/function.fopen.php
- PHP公式ドキュメント - fwrite関数：https://www.php.net/manual/en/function.fwrite.php