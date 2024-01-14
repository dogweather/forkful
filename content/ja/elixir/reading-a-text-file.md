---
title:    "Elixir: テキストファイルを読む"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜ
テキストファイルを読み込むことがなぜ重要なのか？今回はElixir言語でのテキストファイル読み込みについて紹介します。

## 方法
テキストファイルを読み込むには、Fileモジュールを使います。以下はコード例です。

```Elixir
File.read("example.txt") # ファイルの内容を文字列として返す
File.read!("example.txt") # ファイルが存在しない場合は例外を発生させる
File.read?("./nonexistent.txt") # ファイルが存在しない場合はnilを返す
```

テキストファイルの内容を1行ずつ読み込むには、File.streamを使います。以下はコード例です。

```Elixir
File.stream!("example.txt") # ファイルの内容をストリームとして返す
|> Stream.each(&IO.puts(&1)) # 各行を表示する
|> Stream.run # ストリームを実行する
```

以上のように、Elixirでは簡単にテキストファイルを読み込むことができます。また、ファイルの書き込みも同様にFileモジュールを使って行うことができます。

## ディープダイブ
テキストファイルを読み込む際には、エンコーディングや改行コードなどの設定にも注意する必要があります。また、大きなファイルを処理する際にはメモリの使用にも注意が必要です。詳細については、Elixirの公式ドキュメントを参照してください。

## 参考リンク
- [Elixir Fileモジュール](https://hexdocs.pm/elixir/File.html)
- [Elixir 文字コード変換](https://hexdocs.pm/elixir/String.html#to_char_list/2)
- [Elixir ストリーム処理](https://hexdocs.pm/elixir/Stream.html)