---
title:                "ディレクトリが存在するかを確認する"
html_title:           "Gleam: ディレクトリが存在するかを確認する"
simple_title:         "ディレクトリが存在するかを確認する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

あなたはディレクトリが存在するかどうかを確認するのに興味があるかもしれません。これは、ファイルやデータベースの作成、読み書き、編集などのプログラミングタスクを行う際に、重要なステップです。

## 方法

ディレクトリの存在をチェックする方法は、Gleamの"File"モジュールを使用することです。下記のコード例を参考にしてください。

```Gleam
import File

// ディレクトリが存在するかどうかをチェック
let result = File.exists("path/to/directory")

// 結果を表示
IO.print("Directory exists: " ++ String.to_bool_string(result))
```

上記のコードを実行すると、指定したディレクトリが存在する場合には"Directory exists: true"、存在しない場合には"Directory exists: false"と表示されます。

詳細な情報やオプションパラメータを使用する方法については、Gleamの公式ドキュメントをご参照ください。

## ディープダイブ

ディレクトリのチェックには、単純に存在を確認するだけでなく、指定したディレクトリが実際にディレクトリであるかどうかを確認することもできます。また、オプションパラメータを使用することで、パーミッションのチェックや非同期の処理も行えます。

## 詳しくは以下を参照

- [Gleam 公式ドキュメント - Fileモジュール](https://gleam.run/modules/file/)
- [Gleam 公式ドキュメント - File.exists関数](https://gleam.run/guide/functions/)
- [Gleam 公式ドキュメント - ファイルまたはディレクトリの作成方法](https://gleam.run/guide/file-io/)