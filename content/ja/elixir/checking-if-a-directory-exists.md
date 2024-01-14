---
title:    "Elixir: ディレクトリが存在するかどうかを確認する"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜディレクトリの存在をチェックするのか

ディレクトリの存在をチェックすることは、プログラムの安全性を確保するために非常に重要です。もしプログラムが存在しないディレクトリにファイルを保存しようとした場合、エラーが発生しプログラムがクラッシュする可能性があります。そのため、ディレクトリの存在をチェックすることでプログラムがスムーズに実行されるようにすることができます。

## ディレクトリの存在をチェックする方法

以下の例では、ディレクトリが存在するかどうかを確認するためのElixirコードを示します。

```elixir
if File.exists?("directory_path") do
  IO.puts("Directory exists!")
else
  IO.puts("Directory does not exist.")
end
```

このコードでは、`File.exists?`関数を使用してディレクトリが存在するかどうかをチェックしています。もしディレクトリが存在する場合は`Directory exists!`という出力が、存在しない場合は`Directory does not exist.`という出力が得られます。

## ディレクトリの存在をチェックするには

ディレクトリの存在をチェックするには、`File.exists?`以外にも`File.stat`や`File.ls`のような関数を使用することができます。また、条件分岐の代わりに例外処理を使うこともできます。いずれの方法を使用しても、ディレクトリの存在を確認することができます。

## See Also （関連リンク）

- [Elixir Documentation - File module](https://hexdocs.pm/elixir/File.html)
- [Elixir Documentation - Error handling](https://elixir-lang.org/getting-started/error-handling.html)
- [Erlang Documentation - File system functions](https://erlang.org/doc/man/file.html)