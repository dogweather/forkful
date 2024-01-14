---
title:                "Elixir: 一時ファイルを作成する。"
simple_title:         "一時ファイルを作成する。"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

なぜ：一時ファイルを作成する理由

一時ファイルを作成する理由は、一時的にデータを保存するためです。例えば、プログラムの実行中に作成されたデータを一時的に保存したい場合や、一時的なファイルを使用してプログラムの実行中に他のアクションを続行したい場合などが挙げられます。

## 作り方

一時ファイルを作成するためには、`Tempfile`モジュールを使用します。

```
Elixir
# Tempfileモジュールを使用
{:ok, file} = Tempfile.open()

# ファイルにデータを書き込む
IO.write(file, "Hello, World!")

# ファイルを閉じる
File.close(file)

# ファイルパスを取得
IO.puts(file.path)
# => "/tmp/elixirtempfile20190415-1"
```

上記の例では、`Tempfile.open()`メソッドを使用して一時ファイルを作成し、`IO.write()`メソッドを使用してファイルにデータを書き込み、最後に`File.close()`メソッドを使用してファイルを閉じています。最後に、`file.path`を使用してファイルのパスを取得しています。

## ディープダイブ

一時ファイルを作成する際に使用する`Tempfile`モジュールは、様々なオプションを提供しています。例えば、ファイルの作成場所を指定したり、ファイルの名前をカスタマイズしたりすることができます。また、一時ファイルを作成する際に使用されるテンプレートのフォーマットをカスタマイズすることもできます。詳細については、[Elixir公式ドキュメント](https://hexdocs.pm/elixir/Tempfile.html)を参照してください。

## See Also

- [Elixir 公式ドキュメント](https://hexdocs.pm/elixir/Tempfile.html)
- [TempfileモジュールのGitHubリポジトリ](https://github.com/elixir-lang/elixir/blob/master/lib/tempfile.ex)
- [一時ファイルを使用する場合のセキュリティについて](https://www.schneier.com/blog/archives/2006/11/safely_using_te.html)