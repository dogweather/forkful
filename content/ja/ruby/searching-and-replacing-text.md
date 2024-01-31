---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:58:32.136502-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

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
