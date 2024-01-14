---
title:    "Elixir: ディレクトリが存在するかどうかをチェックする"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

Elixirは、優れたエラー処理機能を備えた強力なプログラミング言語です。この言語を使っている場合、ディレクトリが存在するかどうかをチェックすることは、プログラムの安定性を保つために非常に重要です。

## 方法

まず、File.stat/1というElixirの関数を使用して指定されたディレクトリの情報を取得します。これにより、ディレクトリが存在するかどうかを確認できます。次に、Elixirの条件分岐を使用して、ディレクトリが存在する場合は"exists"、存在しない場合は"does not exist"という結果を出力します。

```Elixir
def check_dir(directory) do
  dir_info = File.stat(directory)
  if dir_info.type == :directory do
    IO.puts "The directory exists."
  else
    IO.puts "The directory does not exist."
  end
end
```

ディレクトリが存在する場合の出力結果は次のようになります。「test_dir」は存在するディレクトリの名前です。

```
iex> check_dir("test_dir")
The directory exists.
:ok
```

ディレクトリが存在しない場合の出力結果は次のようになります。

```
iex> check_dir("non_existent_dir")
The directory does not exist.
:ok
```

Elixirの関数を使用することで、簡単にディレクトリの存在を確認できます。

## 深い情報

Elixirでは、File.stat/1の他にもディレクトリの存在を確認するための機能があります。例えば、File.ls!/1という関数を使用すると、指定したディレクトリ内のファイルやサブディレクトリのリストを取得できます。また、File.cwd/0を使用すると、現在のディレクトリのパスを取得することもできます。これらの機能を組み合わせることで、より詳細なディレクトリの情報を取得することができます。

## 参考リンク

- Elixir 公式ドキュメント: https://elixir-lang.org/getting-started/introduction.html
- File.stat/1: https://hexdocs.pm/elixir/File.html#stat/1
- File.ls!/1: https://hexdocs.pm/elixir/File.html#ls!/1
- File.cwd/0: https://hexdocs.pm/elixir/File.html#cwd/0