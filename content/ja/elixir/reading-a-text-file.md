---
title:                "テキストファイルの読み込み"
html_title:           "Elixir: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何 ＆ なぜ？

テキストファイルを読み込むとは、プログラマーがファイル内の情報をコンピューターに取り込むことです。プログラマーはこの技術を使用して、例えばデータ処理やプログラムの構成など、多様な目的のためにファイルから必要な情報を取得することができます。

## 方法：

```Elixir
File.read("example.txt")

#=> {:ok, "ファイルの内容"}
```

```Elixir
IO.binread("example.txt")

#=> "ファイルの内容"
```

## 深堀：

テキストファイルを読み込む方法は、プログラミング言語によって異なりますが、基本的な理念は同じです。大昔はパンチカードやテープなどの物理メディアを使用して情報を読み取っていましたが、現代ではデータをテキストファイルに保存し、プログラムを使用して処理する方法が一般的です。Elixirでは、```File.read/1```と```IO.binread/1```がファイルを読み込むための主な関数です。これらの関数はファイルを開き、その内容をコンピューター内のデータに変換し、プログラムがその情報を利用できるようにします。

## 関連情報：

- [Elixir 公式ドキュメント（英語）](https://elixir-lang.org/getting-started/readme.html#reading-files)
- [Elixir Schoolでのファイル操作の解説（英語）](https://elixirschool.com/ja/lessons/basics/io/#boy)
- [Elixirと他のプログラミング言語におけるファイル操作の比較（英語）](https://www.freecodecamp.org/news/file-handling-in-elixir-vs-python-vs-ruby/)