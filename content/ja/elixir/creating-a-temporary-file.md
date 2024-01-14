---
title:                "Elixir: 「一時ファイルの作成」"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#なぜ
一時的なファイルを作成することについて *なぜ* 人々が取り組むのか、その理由を1-2文で説明します。

一時的なファイルを作成する必要があるかもしれません。例えば、アプリケーションでデータを一時的に保存する必要がある場合や、ファイルの一時的なバックアップを作成する必要がある場合などです。

##やりかた

```Elixir
{:ok, file} = File.open_temp() 
|> File.write("This is a temporary file.")
|> File.close()

IO.inspect file.path
```

実行すると、現在の作業ディレクトリに一時的なファイルが作成され、そのパスが表示されます。このファイルはプロセスが終了すると自動的に削除されます。

##ディープダイブ

一時的なファイルの作成には、Elixirに組み込まれている `File` モジュールが使用されます。このモジュールには、一時的なファイルを作成し書き込むための様々な関数が用意されています。例えば、 `File.open_temp/2` は一時的なファイルを作成してそのリソースを返します。また、`File.write/2` を使用することでファイルにデータを書き込むことができます。

#参考リンク
- [Elixir Fileモジュールドキュメント](https://hexdocs.pm/elixir/File.html)
- [一時ファイル作成時にエラーを避ける方法](https://stackoverflow.com/questions/43549955/how-can-i-avoid-getting-an-error-could-not-locally-open-save-edited-file-no-s)