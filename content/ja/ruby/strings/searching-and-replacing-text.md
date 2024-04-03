---
date: 2024-01-20 17:58:32.136502-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u3068\u306F\
  \u3001\u7279\u5B9A\u306E\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u4ED6\u306E\
  \u6587\u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\
  \u3053\u308C\u306F\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u30D0\u30B0\u306E\u4FEE\
  \u6B63\u3001\u30B3\u30FC\u30C9\u306E\u66F4\u65B0\u4F5C\u696D\u3092\u52B9\u7387\u5316\
  \u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\u3088\u304F\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.831211-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u3068\u306F\
  \u3001\u7279\u5B9A\u306E\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u4ED6\u306E\
  \u6587\u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\
  \u3053\u308C\u306F\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u30D0\u30B0\u306E\u4FEE\
  \u6B63\u3001\u30B3\u30FC\u30C9\u306E\u66F4\u65B0\u4F5C\u696D\u3092\u52B9\u7387\u5316\
  \u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\u3088\u304F\u884C\
  \u3044\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## What & Why?
テキストの検索と置換とは、特定の文字列を見つけて他の文字列で置き換えることです。これはデータの整形、バグの修正、コードの更新作業を効率化するためにプログラマがよく行います。

## How to:
```Ruby
text = "こんにちは, 世界!"
search = "世界"
replace = "Ruby"

# 文字列を置換する（gsubメソッドの使用)
new_text = text.gsub(search, replace)
puts new_text  # => "こんにちは, Ruby!"

# 正規表現を使った置換
regex_text = "今日は2023年3月30日です。"
new_regex_text = regex_text.gsub(/\d{4}年\d{1,2}月\d{1,2}日/, 'XXXX年XX月XX日')
puts new_regex_text  # => "今日はXXXX年XX月XX日です。"
```

## Deep Dive
テキスト置換は古いタイプライター時代からある。コンピュータでは初期のエディタからこの機能が備わっていた。`gsub`（global substitution）はRubyの強力なメソッドで、文字列や正規表現パターンを使った置換が可能。代替手段としては、`sub`メソッドがあり、これは最初に見つかったインスタンスのみを置換する。正規表現を使えば、より複雑な検索・置換パターンを実現できる。パフォーマンス最適化には、不必要な置換ロジックを避け、正規表現を慎重に使うことが大事。

## See Also
- [Rubyのドキュメント](https://docs.ruby-lang.org/ja/)
- [gsubの詳細](https://docs.ruby-lang.org/ja/latest/method/String/i/gsub.html)
- [オンラインRubyコンパイラ](https://replit.com/languages/ruby)
