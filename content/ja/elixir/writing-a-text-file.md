---
title:                "Elixir: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜテキストファイルを書くのか

テキストファイルを書くことは、プログラミングにとって非常に重要なスキルです。テキストファイルには、プログラムのコード、設定ファイル、ドキュメント、メモなど、さまざまな情報を保存することができます。それでは、Elixirでテキストファイルをどのように書くのか見ていきましょう。

## 書き方

まずは、Elixirでテキストファイルを書く方法を見ていきましょう。下のコードブロックに例を示します。

```Elixir
# テキストファイルを書き込む関数を定義する
def write_to_file(file_name, content) do
  File.write(file_name, content)
end

# テキストファイルに書き込む内容を定義する
content = "これはテキストファイルに書き込む内容です。"

# ファイル名と内容を渡してテキストファイルを書き込む
write_to_file("my_file.txt", content)

# テキストファイルから内容を読み取り、出力する
File.read("my_file.txt")
```

上の例では、`write_to_file`という関数を定義し、その中で`File.write`を使用してテキストファイルを書き込んでいます。また、`content`という変数に書き込む内容を定義し、`write_to_file`関数に渡しています。最後に、`my_file.txt`というファイル名でテキストファイルを書き込み、`File.read`を使用して内容を読み取り出力しています。

## 深堀り

Elixirでテキストファイルを書くには、`File.write`以外にも`IO.binwrite`や`IO.write`などの関数があります。これらの関数を使用する際には、エンコーディングやフォーマットなどのオプションを指定することで、より詳細な設定が可能です。また、`File.read`や`IO.read`を使用することで、ファイルから内容を読み取ることもできます。

さらに、Elixirでは`File`モジュールの他にも、`:file`や`IO`などのモジュールを使用してファイル操作を行うことができます。各モジュールにはそれぞれ異なる関数やオプションが用意されており、使用目的や状況に応じて使い分けることができます。

## 関連リンク

- [Elixir 公式ドキュメント](https://elixir-lang.org/docs.html)
- [Elixir School](https://elixirschool.com/ja/)
- [Elixir Forum](https://elixirforum.com/)
- [Elixir入門書「プログラミングElixir」](https://booth.pm/ja/items/990593)
- [Elixirクックブック](https://booth.pm/ja/items/915729)
- [Elixir 入門記事まとめ](https://qiita.com/tags/elixir)
- [Elixir Advent Calendar](https://qiita.com/advent-calendar/2019/elixir)