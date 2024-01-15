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

## なぜ？

テキストファイルを読み取ることは、プログラムの開発やデータ処理の一部を担当する人々にとって重要なスキルです。テキストファイルを読むことで、プログラムに必要なデータや情報を簡単に取得することができます。

## 方法

まずはElixirの `File` モジュールを使って、テキストファイルを開きます。

```
File.read("sample.txt")
```

テキストファイルの中身をすべて読み込む場合は、 `File.read!/1` を使います。

```
File.read!("sample.txt")
```

テキストファイルを1行ずつ読み込みたい場合は、 `File.stream!/1` を使用することができます。

```
File.stream!("sample.txt") |> Stream.each(&IO.puts/1)
```

## 深堀り

Elixirでは、テキストファイルを読み込むことができるだけでなく、パスやサイズなどのファイルに関するさまざまな情報を取得することもできます。また、文字エンコーディングや改行コードなどのオプションを指定することもできます。

## 関連記事

- [Elixirの公式ドキュメント](https://hexdocs.pm/elixir/File.html#read!/1)
- [テキストファイルの読み書きについてのElixir Forumのディスカッションスレッド](https://elixirforum.com/t/reading-writing-text-files-in-elixir/926)
- [「プログラミングElixir」の第3章 - テキストファイル操作](https://www.amazon.co.jp/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0Elixir-%E7%AC%AC3%E7%AB%A0-%E3%83%86%E3%82%AD%E3%82%B9%E3%83%88%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E6%93%8D%E4%BD%9C-Dave-Thomas/dp/4274219153)