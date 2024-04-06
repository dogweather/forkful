---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:20.895447-07:00
description: "\u4F7F\u3044\u65B9\uFF1A \u6587\u5B57\u5217\u3092\u5358\u7D14\u306A\u30D1\
  \u30BF\u30FC\u30F3\u306B\u5BFE\u3057\u3066\u30DE\u30C3\u30C1\u3055\u305B\u308B\u306B\
  \u306F\u3001`match`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\
  \u3002\u4EE5\u4E0B\u3067\u306F\u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\
  \u306E\u4E2D\u306B\u300CRuby\u300D\u3068\u3044\u3046\u5358\u8A9E\u304C\u5B58\u5728\
  \u3059\u308B\u304B\u30C1\u30A7\u30C3\u30AF\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.837518-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5358\u7D14\u306A\u30D1\u30BF\u30FC\u30F3\u306B\
  \u5BFE\u3057\u3066\u30DE\u30C3\u30C1\u3055\u305B\u308B\u306B\u306F\u3001`match`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u3067\
  \u306F\u3001\u4E0E\u3048\u3089\u308C\u305F\u6587\u5B57\u5217\u306E\u4E2D\u306B\u300C\
  Ruby\u300D\u3068\u3044\u3046\u5358\u8A9E\u304C\u5B58\u5728\u3059\u308B\u304B\u30C1\
  \u30A7\u30C3\u30AF\u3057\u3066\u3044\u307E\u3059."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 使い方：


### 基本的なマッチング
文字列を単純なパターンに対してマッチさせるには、`match`メソッドを使用できます。以下では、与えられた文字列の中に「Ruby」という単語が存在するかチェックしています。

```ruby
if /Ruby/.match("Hello, Ruby!")
  puts "Match found!"
end
# 出力: Match found!
```

### 変数を使ったパターンマッチング
`#{}`構文を使って変数を正規表現に埋め込むことで、パターンを動的にすることができます。

```ruby
language = "Ruby"
if /#{language}/.match("Programming in Ruby is fun.")
  puts "Talking about Ruby!"
end
# 出力: Talking about Ruby!
```

### 正規表現を使った置換
`gsub`メソッドを使うと、指定された置換文字列でパターンのすべての出現を置き換えることができます。

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# 出力: barbarbar
```

### キャプチャリング
正規表現内の括弧は、マッチの一部をキャプチャするために使われます。`match`メソッドは`MatchData`オブジェクトを返し、キャプチャにアクセスするために使用できます。

```ruby
match_data = /(\w+): (\d+)/.match("Age: 30")
puts match_data[1] # キャプチャされたラベル
puts match_data[2] # キャプチャされた値
# 出力:
# Age
# 30
```

### サードパーティライブラリの使用
Rubyの標準ライブラリは強力ですが、時にはより専門的な機能が必要になる場合があります。正規表現を使用するための人気のあるgemの1つに`Oniguruma`があり、ビルトインのRuby正規表現エンジンを超える追加の正規表現機能を提供します。

次でインストールします：
```bash
gem install oniguruma
```

例えば、以下のように使用できます（`oniguruma`をインストールした後に必要となる設定を行っていると仮定します）：

```ruby
# これはより高度な例であり、追加の設定が必要になる場合があります
require 'oniguruma'

pattern = Oniguruma::ORegexp.new('(\d+)')
match_data = pattern.match("The number is 42.")
puts match_data[1]
# 出力: 42
```

覚えておいてください、強力である一方で、正規表現はより複雑なパターンになると管理が難しくなることがあります。可読性を目指し、正規表現が複雑になりすぎた場合は代替方法を検討してください。
