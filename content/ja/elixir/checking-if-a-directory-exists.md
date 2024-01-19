---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Elixir: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認するとは、特定のディレクトリの存在を確認するプログラミングタスクのことです。プログラマーはこれを行うことで、エラーや特定のタスクの失敗を防ぎます。

## 使い方：

以下のElixirのコードは、特定のディレクトリが存在するかどうかを確認します：
```Elixir
defmodule DirectoryExists do
  def check(directory_path) do
    File.dir?(directory_path)
  end
end

IO.puts DirectoryExists.check("/path/to/directory")
```
これを実行すると、指定したディレクトリが存在する場合は`true`、存在しない場合は`false`が出力されます。

## 深掘り：

ディレクトリ存在のチェックは、過去の多くのプログラミングでしばしば使われる基本的な操作でした。しかし、近年では一部のシステムではこの操作が不必要または偽の結果を返す可能性があるため、最善のプラクティスとは言えません。その代わりに、必要な操作を試して失敗したらチェックを行うというアプローチが推奨されます。

あなたが探しているディレクトリが存在しない場合、`File.dir?`は`false`を返します。これは、指定したパスが存在しないか、または存在はしているがディレクトリではない場合に発生します。

## 関連情報：

* Elixir `File` モジュールドキュメント: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
* Elixirのエラーハンドリングについての情報: [http://elixir-recipes.github.io/error-handling/](http://elixir-recipes.github.io/error-handling/) 

以上の内容を理解して、ディレクトリの存在チェックをうまく活用しましょう。