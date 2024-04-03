---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:12.735079-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Elixir\u306E\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306F\u3001`File`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u901A\u3058\
  \u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u7C21\u5358\u306B\
  \u78BA\u8A8D\u3059\u308B\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u3068\
  \u304A\u308A\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.630631-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001`File`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u901A\u3058\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\
  \u306E\u5B58\u5728\u3092\u7C21\u5358\u306B\u78BA\u8A8D\u3059\u308B\u65B9\u6CD5\u3092\
  \u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\
  \u308B\u65B9\u6CD5\u306F\u6B21\u306E\u3068\u304A\u308A\u3067\u3059\uFF1A."
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
