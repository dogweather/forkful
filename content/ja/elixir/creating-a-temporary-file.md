---
title:    "Elixir: 一時ファイルの作成"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜ

一時的なファイルを作成する理由は何でしょうか？実際、私たちが何かをプログラムする際には、一時的なファイルを作成する必要があることがあります。それは、プログラムが実行される際に一時的に使用されるデータを格納するためです。Elixirでは、一時的なファイルを作成することが非常に簡単です。

## 作り方

まず、Elixirで一時的なファイルを作成する方法を見てみましょう。以下のコード例では、一時的なファイルを作成し、そのファイルを開き、内容を書き込んで、最後にファイルを閉じる方法を示します。

```Elixir
{:ok, file} = File.open("my_temp_file.txt", [:write, :utf8])
IO.write(file, "これは一時的なファイルです。")
File.close(file)
```

上記の例では、`File.open`関数を使用してファイルを開き、ファイルの内容を書き込んでから閉じることで一時的なファイルを作成しています。また、`:write`オプションを使用して、ファイルを書き込みモードで開いています。

## 深堀り

一時的なファイルを作成する方法を見てきましたが、実際には`File`モジュールには`tempfile/2`という関数が用意されています。この関数を使用することで、一時的なファイルを作成することができます。例えば、以下のように使用することができます。

```Elixir
{:ok, pid} = File.tempfile("my_temp_file")
IO.puts("一時的なファイルを作成しました。ファイルのパスは: #{pid}")
```

`tempfile/2`関数では、引数として保存先のパスを指定することができます。また、`File`モジュールには`temp_dir/0`関数もあり、一時的なファイルを作成するディレクトリを指定することができます。

## 参考リンク

- [Elixir - File](https://hexdocs.pm/elixir/File.html)
- [Elixir - IO](https://hexdocs.pm/elixir/IO.html)
- [Elixir - Kernel. open/2](https://hexdocs.pm/elixir/Kernel.html#open/2)
- [Elixir - Kernel.temp_dir/0](https://hexdocs.pm/elixir/Kernel.html#temp_dir/0)