---
title:                "Gleam: 「ディレクトリが存在するかどうかを確認する」"
simple_title:         "「ディレクトリが存在するかどうかを確認する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

今回は、Gleamでディレクトリが存在するかどうかをチェックする方法について紹介します。

## なぜチェックする必要があるのか？

ディレクトリが存在するかどうかをチェックすることは、プログラムの処理をより効率的に行うために重要です。例えば、処理を行う前にディレクトリが存在するかどうかを確認することで、エラーを防ぐことができます。

## 方法

ディレクトリが存在するかどうかをチェックするには、```is_dir```関数を使用します。以下の例を参考にしてください。

```Gleam
import gleam/io

let dir_exists = io.is_dir("path/to/directory")
io.println(dir_exists)
```

出力は、```true```または```false```となります。

## 詳細を調べる

```is_dir```関数は、内部的にファイルを開いてチェックを行います。そのため、ファイルが存在しない場合にはエラーが発生します。また、パーミッションの問題がある場合にもエラーが発生します。

より詳細な情報を知りたい場合は、Gleamのドキュメントを参照してください。

## 関連リンク

[ファイルを読み込む方法（Gleam公式ドキュメント）](https://gleam.run/documentation/writing-files)

[パーミッションを設定する方法（Gleam公式ドキュメント）](https://gleam.run/documentation/file-permissions)