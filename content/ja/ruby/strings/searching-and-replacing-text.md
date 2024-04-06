---
date: 2024-01-20 17:58:32.136502-07:00
description: "How to: \u30C6\u30AD\u30B9\u30C8\u7F6E\u63DB\u306F\u53E4\u3044\u30BF\
  \u30A4\u30D7\u30E9\u30A4\u30BF\u30FC\u6642\u4EE3\u304B\u3089\u3042\u308B\u3002\u30B3\
  \u30F3\u30D4\u30E5\u30FC\u30BF\u3067\u306F\u521D\u671F\u306E\u30A8\u30C7\u30A3\u30BF\
  \u304B\u3089\u3053\u306E\u6A5F\u80FD\u304C\u5099\u308F\u3063\u3066\u3044\u305F\u3002\
  `gsub`\uFF08global\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.623979-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u7F6E\u63DB\u306F\u53E4\u3044\u30BF\u30A4\u30D7\
  \u30E9\u30A4\u30BF\u30FC\u6642\u4EE3\u304B\u3089\u3042\u308B\u3002\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u3067\u306F\u521D\u671F\u306E\u30A8\u30C7\u30A3\u30BF\u304B\u3089\
  \u3053\u306E\u6A5F\u80FD\u304C\u5099\u308F\u3063\u3066\u3044\u305F\u3002`gsub`\uFF08\
  global substitution\uFF09\u306FRuby\u306E\u5F37\u529B\u306A\u30E1\u30BD\u30C3\u30C9\
  \u3067\u3001\u6587\u5B57\u5217\u3084\u6B63\u898F\u8868\u73FE\u30D1\u30BF\u30FC\u30F3\
  \u3092\u4F7F\u3063\u305F\u7F6E\u63DB\u304C\u53EF\u80FD\u3002\u4EE3\u66FF\u624B\u6BB5\
  \u3068\u3057\u3066\u306F\u3001`sub`\u30E1\u30BD\u30C3\u30C9\u304C\u3042\u308A\u3001\
  \u3053\u308C\u306F\u6700\u521D\u306B\u898B\u3064\u304B\u3063\u305F\u30A4\u30F3\u30B9\
  \u30BF\u30F3\u30B9\u306E\u307F\u3092\u7F6E\u63DB\u3059\u308B\u3002\u6B63\u898F\u8868\
  \u73FE\u3092\u4F7F\u3048\u3070\u3001\u3088\u308A\u8907\u96D1\u306A\u691C\u7D22\u30FB\
  \u7F6E\u63DB\u30D1\u30BF\u30FC\u30F3\u3092\u5B9F\u73FE\u3067\u304D\u308B\u3002\u30D1\
  \u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u6700\u9069\u5316\u306B\u306F\u3001\u4E0D\u5FC5\
  \u8981\u306A\u7F6E\u63DB\u30ED\u30B8\u30C3\u30AF\u3092\u907F\u3051\u3001\u6B63\u898F\
  \u8868\u73FE\u3092\u614E\u91CD\u306B\u4F7F\u3046\u3053\u3068\u304C\u5927\u4E8B\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
