---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:55.819487-07:00
description: "\u6587\u5B57\u5217\u306E\u5148\u982D\u3092\u5927\u6587\u5B57\u306B\u5909\
  \u63DB\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u4FDD\
  \u3064\u3053\u3068\u3092\u6587\u5B57\u5217\u306E\u30AD\u30E3\u30D4\u30BF\u30E9\u30A4\
  \u30BA\uFF08\u5148\u982D\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3053\
  \u3068\uFF09\u3068\u8A00\u3044\u307E\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\
  \u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\
  \u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3067\u306E\
  \u30C6\u30AD\u30B9\u30C8\u8868\u793A\u306A\u3069\u3001\u4E00\u8CAB\u6027\u3068\u53EF\
  \u8AAD\u6027\u304C\u91CD\u8981\u306A\u5834\u9762\u3067\u3088\u304F\u5FC5\u8981\u3068\
  \u3055\u308C\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.590451-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u5148\u982D\u3092\u5927\u6587\u5B57\u306B\u5909\
  \u63DB\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u4FDD\
  \u3064\u3053\u3068\u3092\u6587\u5B57\u5217\u306E\u30AD\u30E3\u30D4\u30BF\u30E9\u30A4\
  \u30BA\uFF08\u5148\u982D\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3053\
  \u3068\uFF09\u3068\u8A00\u3044\u307E\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\
  \u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\
  \u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3067\u306E\
  \u30C6\u30AD\u30B9\u30C8\u8868\u793A\u306A\u3069\u3001\u4E00\u8CAB\u6027\u3068\u53EF\
  \u8AAD\u6027\u304C\u91CD\u8981\u306A\u5834\u9762\u3067\u3088\u304F\u5FC5\u8981\u3068\
  \u3055\u308C\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

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
