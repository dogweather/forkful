---
title:                "文字列を大文字にする"
aliases:
- ja/elixir/capitalizing-a-string.md
date:                  2024-02-03T19:04:55.819487-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

文字列の先頭を大文字に変換し、残りの文字を小文字に保つことを文字列のキャピタライズ（先頭文字を大文字にすること）と言います。この操作は、ユーザー入力のフォーマットやユーザーインターフェイスでのテキスト表示など、一貫性と可読性が重要な場面でよく必要とされます。

## 方法：

Elixirは、サードパーティライブラリを必要とせずに、ビルトイン関数を使用して文字列をキャピタライズする簡単な方法を提供します。以下は簡単な例です：

```elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

出力：

```
Elixir programming
```

より複雑なキャピタライズのロジックやコントロールが必要な場合には、異なるString関数を組み合わせて使用することもできます。例えば、文中のすべての単語をキャピタライズする必要がある場合、文を単語に分割し、各単語をキャピタライズしてから再び結合します：

```elixir
sentence = "elixir is fun"
capitalized_sentence = sentence 
                        |> String.split() 
                        |> Enum.map(&String.capitalize/1) 
                        |> Enum.join(" ")

IO.puts capitalized_sentence
```

出力：

```
Elixir Is Fun
```

Elixirの標準ライブラリがほとんどのニーズをカバーしている一方で、より繊細なテキスト操作、特に進んだ文字列のキャピタライズに関しては、国際化を扱うCldrのようなサードパーティライブラリを探求することもできます。これらはロケール固有のキャピタライズの挙動を提供することがあります。
