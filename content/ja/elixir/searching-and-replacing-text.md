---
title:    "Elixir: テキストの検索と置換"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜ

文字列を検索して置換することに関わる理由は、プログラマーがテキストデータを操作する必要があるからです。たとえば、データベースやファイルから特定の文字列を見つけて、別の文字列に置き換える必要がある場合があります。

## やり方

文字列の検索と置換は、Elixirの`String.replace`関数を使用することで簡単に行うことができます。以下のように、検索する文字列と置換する文字列を指定するだけで、新しい文字列が作成されます。

```Elixir
original_string = "こんにちは、世界"
replaced_string = String.replace(original_string, "世界", "Elixir")

IO.puts(replaced_string)

# 出力: こんにちは、Elixir
```

また、正規表現を使用することで、より柔軟な検索と置換を行うこともできます。正規表現は`Regex`モジュールを使用して作成できます。

```Elixir
original_string = "Elixirはとても便利です"
replaced_string = String.replace(original_string, ~r/(Elixir)/, "これ")

IO.puts(replaced_string)

# 出力: これはとても便利です
```

## ディープダイブ

検索と置換をより詳しく理解するためには、文字列のパターンマッチングや正規表現の仕組みを学ぶことが重要です。

文字列を比較する際に使用されるパターンマッチングは、文字列の部分一致を検索するために使用できます。例えば、以下のように、文字列が特定のパターンを含むかどうかを調べることができます。

```Elixir
original_string = "こんにちは、世界"

if String.contains?(original_string, "世界") do
  IO.puts("世界が含まれています")
end

# 出力: 世界が含まれています
```

正規表現は、さまざまなパターンにマッチする機能を持つ強力なツールです。正規表現を使用することで、繰り返し構造や文字列の一部を動的に指定することができます。正規表現の詳細については、Elixirの公式ドキュメントを参照してください。

## See Also

- [Elixirの公式ドキュメント](https://elixir-lang.org/getting-started/string-pattern-matching-and-replacing.html)
- [正規表現についての詳細な説明](https://qiita.com/jnchito/items/c7e6fcf579e757d4dbce)
- [正規表現の練習問題](https://tutorial.math.lamar.edu/files/livedocs/m8007/exercises/exaexchexregexpr.aspx)