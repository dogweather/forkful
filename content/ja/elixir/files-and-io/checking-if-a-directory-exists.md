---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:12.735079-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.630631-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\
  \u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u5185\u306E\u7279\u5B9A\u306E\u30D1\
  \u30B9\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u691C\u8A3C\
  \u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30C7\
  \u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u6B20\u5982\u306B\u3088\u308B\u30A8\u30E9\u30FC\
  \u306B\u906D\u9047\u3059\u308B\u3053\u3068\u306A\u304F\u3001\u5B89\u5168\u306B\u305D\
  \u306E\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304B\u3089\u8AAD\u307F\u53D6\u308A\u3001\
  \u66F8\u304D\u8FBC\u307F\u3001\u307E\u305F\u306F\u64CD\u4F5C\u3092\u884C\u3046\u3053\
  \u3068\u304C\u3067\u304D\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3057\u307E\
  \u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## どうやって：
Elixirの標準ライブラリは、`File`モジュールを通じてディレクトリの存在を簡単に確認する方法を提供しています。これを使用する方法は次のとおりです：

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Directory exists!"
else
  IO.puts "Directory does not exist."
end
```

ディレクトリが存在しないと仮定した場合のサンプル出力：
```
Directory does not exist.
```

ディレクトリの存在を確認することを含む、より高度なファイルシステムのやり取りを行いたい場合は、`FileSystem`のようなサードパーティのライブラリの使用を検討するかもしれません。Elixirの標準機能は多くのケースに対して十分ですが、`FileSystem`は複雑なアプリケーションに対してより繊細な制御とフィードバックを提供できる可能性があります。しかし、ディレクトリが存在するかどうかを確認するという基本的なニーズについては、外部の依存関係を必要とせず、すぐに使用できるため、ネイティブの`File`モジュールを使用することが一般的に推奨されます。
