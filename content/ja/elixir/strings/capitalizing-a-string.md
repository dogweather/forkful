---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:55.819487-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u306F\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\
  \u30D3\u30EB\u30C8\u30A4\u30F3\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\
  \u5217\u3092\u30AD\u30E3\u30D4\u30BF\u30E9\u30A4\u30BA\u3059\u308B\u7C21\u5358\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\u5358\
  \u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.921722-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Elixir\u306F\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\
  \u30D3\u30EB\u30C8\u30A4\u30F3\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\
  \u5217\u3092\u30AD\u30E3\u30D4\u30BF\u30E9\u30A4\u30BA\u3059\u308B\u7C21\u5358\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u7C21\u5358\
  \u306A\u4F8B\u3067\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

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
