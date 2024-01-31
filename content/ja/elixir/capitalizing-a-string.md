---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なんで & どうして？)
文字列を大文字にすることは、先頭の文字を大文字に、他は小文字に変えることです。読みやすさを向上させたり、固有名詞を強調したりするためによく使われます。

## How to: (やり方)
```Elixir
# Elixirで文字列を大文字にする簡単な例

defmodule StringHelper do
  def capitalize(str) when is_binary(str) do
    String.capitalize(str)
  end
end

# 使用例
IO.puts StringHelper.capitalize("elixir programming")  # 出力: "Elixir programming"
```

## Deep Dive (深掘り)
Elixirの`String.capitalize/1`関数はUnicodeをサポートしていて、世界中の言語で使えます。昔の言語ではASCIIに限定されていたが、Elixirはそれを超えています。`String.upcase/1`や`String.downcase/1`と組み合わせて使用されることもあります。内部的には、Unicode文字の正規化と書式の違いに対応するために複雑な処理をしています。

## See Also (関連情報)
- Elixirの公式ドキュメント [String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/2)
- Unicodeの公式文書 [Unicode Standard](http://www.unicode.org/standard/standard.html)
