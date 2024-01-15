---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Elixir: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Why
## なぜ
ディレクトリが存在するかどうかを確認することの利点は、アプリケーションが正しいファイルパスを使用していることを保証し、エラーを回避することです。 

# How To
## 方法
ディレクトリが存在するかどうかを確認するには、`File.exists?/1`関数を使用します。

```Elixir
File.exists?("path/to/directory")
```

もしくは、`File.cwd?/1`関数を使用して、カレントディレクトリが存在するかどうかを確認することもできます。

```Elixir
File.cwd?("path/to/directory")
```

出力は、`true`もしくは`false`になります。

# Deep Dive
## より詳細な説明
`File.exists?/1`関数は、引数で指定したパスがディレクトリ、ファイル、シンボリックリンクなど存在するかどうかを確認します。もしくは、`File.cwd?/1`関数は、絶対パスか相対パスを引数に取り、カレントディレクトリが指定したパスと同じかどうかを確認します。

# See Also
## 関連記事
- [Elixir 公式ドキュメンテーション] (https://hexdocs.pm/elixir/File.html)
- [Elixir Dir モジュール] (https://hexdocs.pm/elixir/Dir.html)
- [Elixir 標準ライブラリ] (https://hexdocs.pm/elixir/stdlib.html#File.cwd?/1)