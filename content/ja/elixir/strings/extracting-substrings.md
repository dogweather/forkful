---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:45:32.735831-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から特定の一部を取り出すことを、部分文字列を抽出すると言います。これは、データを操作したり、特定の情報を取得・表示したりするためにプログラマーがよく行います。

## How to: (やり方)
```Elixir
# 文字列を定義する
original_string = "こんにちは、エリクサーの世界！"

# 部分文字列を取り出す
substring = String.slice(original_string, 6, 5)

# 結果を表示する
IO.puts(substring)
```
このコードの出力は `エリクサー` になるでしょう。

## Deep Dive (深く掘り下げる)
Elixirでは`String`モジュールを使って文字列を操作します。`String.slice/3`はElixirの初期バージョンから使われてきました。他の言語にも同様の機能がありますが、Elixir特有の並行処理や不変性の文脈で利用されます。また、Elixirの文字列はバイナリとして表現され、UTF-8エンコーディングで扱われるため、バイトサイズが文字数と一致しないことがあります。それを考慮して、`String.slice/3`は文字の境界を正しく扱えるようになっています。

## See Also (関連情報)
- Elixir公式ドキュメント: [String module](https://hexdocs.pm/elixir/String.html)
- Learn Elixir: [文字列とバイナリ](https://elixirschool.com/ja/lessons/basics/strings/)
