---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
テキストファイルの書き込みはデータ保存の基本。プログラムの結果や設定を永続化するために使う。

## How to:

```elixir
# ファイルに "Hello, World!" を書き込む
File.write!("hello.txt", "Hello, World!")

# 書き込み結果を確認
IO.puts(File.read!("hello.txt"))
```

出力:

```
Hello, World!
```

## Deep Dive
Elixirでのファイル書き込みは`File`モジュールを使用。`write/2`や`write!/2`が基本的な関数。この機能はElixirが生まれる前からあるが、ElixirはErlangのVM上で動いており、Erlangの堅牢性を受け継いでいる。別の方法としては、`Stream`を通じて大きなデータを作業することもできるが、小さなファイル向けには`File.write/2`のシンプルさが便利。

## See Also

- [Elixirの公式ドキュメント](https://hexdocs.pm/elixir/File.html)
- [Erlangの :file モジュール](http://erlang.org/doc/man/file.html)
- [Elixirフォーラム](https://elixirforum.com/)
