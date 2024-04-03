---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:04.988669-07:00
description: "\u65B9\u6CD5: Elixir\u306F\u7D44\u307F\u8FBC\u307F\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u3092\u4F7F\u3063\u3066\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u3092\u7C21\
  \u5358\u306B\u3057\u307E\u3059\u3002\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\u8FBC\
  \u3080\u4E3B\u306A\u65B9\u6CD5\u306F\u3001`File.write/2`\u307E\u305F\u306F`File.write!/2`\u95A2\
  \u6570\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u524D\u8005\u306F\
  `:ok`\u307E\u305F\u306F`:error`\u306E\u30BF\u30D7\u30EB\u3092\u8FD4\u3057\u3001\u5F8C\
  \u8005\u306F\u5931\u6557\u6642\u306B\u30A8\u30E9\u30FC\u3092\u767A\u751F\u3055\u305B\
  \u307E\u3059\u3002 \u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A\
  ."
lastmod: '2024-03-13T22:44:41.635185-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u306F\u7D44\u307F\u8FBC\u307F\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\
  \u4F7F\u3063\u3066\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u3092\u7C21\u5358\u306B\u3057\
  \u307E\u3059\u3002\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\u8FBC\u3080\u4E3B\u306A\
  \u65B9\u6CD5\u306F\u3001`File.write/2`\u307E\u305F\u306F`File.write!/2`\u95A2\u6570\
  \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u524D\u8005\u306F`:ok`\u307E\
  \u305F\u306F`:error`\u306E\u30BF\u30D7\u30EB\u3092\u8FD4\u3057\u3001\u5F8C\u8005\
  \u306F\u5931\u6557\u6642\u306B\u30A8\u30E9\u30FC\u3092\u767A\u751F\u3055\u305B\u307E\
  \u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法:
Elixirは組み込みモジュールを使ってファイル操作を簡単にします。ファイルに書き込む主な方法は、`File.write/2`または`File.write!/2`関数を使用することです。前者は`:ok`または`:error`のタプルを返し、後者は失敗時にエラーを発生させます。

こちらが簡単な例です：

```elixir
# ファイルに書き込む、シンプルなメッセージ
File.write("hello.txt", "Hello, World!")

# このコードを実行すると、"Hello, World!"という内容の'hello.txt'が作成されます
```

ファイルに追記する場合は、`File.open/3`を`[:write, :append]`オプションで使用し、その後に`IO.binwrite/2`で内容を追加します：

```elixir
# ファイルに追記する
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nもう一行追加しましょう。")
File.close(file)

# これで'hello.txt'には二行目に"もう一行追加しましょう。"が含まれるようになります。
```

大量のデータを扱う場合や、書き込みプロセスをより詳細にコントロールしたい場合は、`Stream`モジュールを使ってファイルにデータを遅延書き込みすることができます：

```elixir
# 大量のデータセットを遅延書き込みする
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("数字: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# これにより'numbers.txt'が作成され、0から9までの数字がそれぞれ新しい行に書き込まれます。
```

より高度なファイル操作を必要とするプロジェクトの場合、CSVファイル操作のための特化した機能を提供する`CSV`のようなサードパーティのライブラリを検討するかもしれません。しかし、多くの目的において、Elixirの組み込み機能だけで十分であることを忘れないでください。
