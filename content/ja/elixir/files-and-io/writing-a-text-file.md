---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:04.988669-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.635185-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u306F\u958B\u767A\u8005\u306B\u3068\u3063\u3066\u6B20\
  \u304B\u305B\u306A\u3044\u30B9\u30AD\u30EB\u3067\u3059\u3002\u30C7\u30FC\u30BF\u306E\
  \u6C38\u7D9A\u5316\u3001\u30ED\u30B0\u8A18\u9332\u3001\u4EBA\u9593\u304C\u8AAD\u3081\
  \u308B\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u306A\
  \u3069\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u72B6\
  \u614B\u306E\u4FDD\u5B58\u3001\u30C7\u30D0\u30C3\u30B0\u60C5\u5831\u3001\u8A2D\u5B9A\
  \u3001\u307E\u305F\u306F\u30C6\u30AD\u30B9\u30C8\u306E\u3088\u3046\u306A\u666E\u904D\
  \u7684\u306A\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u597D\u3080\u30B7\u30B9\u30C6\
  \u30E0\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306A\u3069\u3092\u9054\u6210\u3059\
  \u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
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
