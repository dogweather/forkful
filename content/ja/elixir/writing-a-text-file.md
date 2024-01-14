---
title:    "Elixir: テキストファイルの作成"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書く理由はさまざまですが、データを保存し、共有し、処理するために非常に便利です。また、Elixirを使ってテキストファイルを書くことで、コードがよりシンプルで処理が高速になります。

## 書き方

以下のコードを使って、Elixirでテキストファイルを書く方法をご紹介します。```Elixir
# sample.txtというファイルを作成
{:ok, file} = File.open("sample.txt", [:write])

# ファイルに書き込むデータを定義
data = "これはテキストファイルに書き込まれた文章です。"

# ファイルにデータを書き込む
IO.write(file, data)

# ファイルを閉じる
File.close(file)
```
このコードを実行すると、"sample.txt"というファイルが作成され、指定した文章が書き込まれます。書き込まれたテキストファイルは、文字コードや改行コードを設定することで、さまざまな言語や環境で読み込むことができます。

## ディープダイブ

Elixirでは、テキストファイルを書き込むために`File.open/2`関数を使用します。`File.open/2`関数の第二引数に`[:write]`を渡すことで、ファイルを書き込みモードで開くことができます。

また、Elixirには`File.write/2`関数や`IO.write/2`関数などもあり、これらを使用することでよりシンプルにテキストファイルを書き込むことができます。

## 参考リンク

この記事では、Elixirでテキストファイルを書き込む方法をご紹介しました。もし他のElixirのファイル操作についても知りたい方は、以下のリンクを参考にしてください。

- [Fileモジュール - Elixir公式ドキュメント](https://hexdocs.pm/elixir/File.html)
- [IOモジュール - Elixir公式ドキュメント](https://hexdocs.pm/elixir/IO.html)
- [QiitaにおけるElixirタグの投稿記事一覧](https://qiita.com/tags/elixir)