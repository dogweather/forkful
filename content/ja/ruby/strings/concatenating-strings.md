---
date: 2024-01-20 17:35:31.301663-07:00
description: "How to: (\u65B9\u6CD5) Ruby\u3067\u6587\u5B57\u5217\u3092\u7D50\u5408\
  \u3059\u308B\u306B\u306F\u3001`+` \u3084 `<<`\u3001`concat`\u3001`#{}`\uFF08\u6587\
  \u5B57\u5217\u5185\u633F\uFF09\u7B49\u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\
  \u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.631854-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Ruby\u3067\u6587\u5B57\u5217\u3092\u7D50\u5408\u3059\u308B\
  \u306B\u306F\u3001`+` \u3084 `<<`\u3001`concat`\u3001`#{}`\uFF08\u6587\u5B57\u5217\
  \u5185\u633F\uFF09\u7B49\u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\u3002\u4F8B\
  \u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (方法)
Rubyで文字列を結合するには、`+` や `<<`、`concat`、`#{}`（文字列内挿）等の方法があります。例を見てみましょう。

```Ruby
# `+`を使った基本的な例
greeting = "こんにちは、" + "世界！"
puts greeting
# 出力: こんにちは、世界！

# `<<` を使ってみましょう。これは破壊的なメソッドです。
location = "Ruby"
location << "プログラミング"
puts location
# 出力: Rubyプログラミング

# `concat` メソッドの使用例
name = "太郎"
name.concat("さん")
puts name
# 出力: 太郎さん

# 文字列内挿を使う方法
item = "本"
price = 1500
puts "#{item}の価格は#{price}円です。"
# 出力: 本の価格は1500円です。
```

## Deep Dive (探求)
文字列結合は初期のプログラミングからあります。Rubyが登場した1995年から、文字列操作は重要な機能です。破壊的な方法（`<<`や`concat`）だと元の文字列自体を変更します。一方、`+`は新しい文字列オブジェクトを作ります。

他の言語では連結に専用の関数や演算子を用いますが、Rubyはオブジェクト指向の柔軟性を活かして多様な方法を提供します。例えば、配列の`join`メソッドを使っても文字列を結合できます。

```Ruby
# 配列`join`メソッドを使った例
words = ["Ruby", "は", "楽しい"]
puts words.join(" ")
# 出力: Ruby は 楽しい
```

`+`はシンプルで理解しやすいが、大量の文字列処理ではメモリの効率が悪くなることがあります。この場合、`<<`や`concat`の使用が推奨されます。

## See Also (関連情報)
- Rubyの公式ドキュメントの[Stringクラス](https://docs.ruby-lang.org/ja/latest/class/String.html)
- [Ruby Style Guide](https://github.com/rubocop/ruby-style-guide#strings)にある文字列結合に関する規約
