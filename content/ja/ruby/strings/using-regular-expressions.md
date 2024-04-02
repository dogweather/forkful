---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:20.895447-07:00
description: "Ruby\u306B\u304A\u3051\u308B\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\
  \u306F\u3001\u6587\u5B57\u5217\u5185\u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\
  \u305B\u3092\u30DE\u30C3\u30C1\u30F3\u30B0\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\
  \u3055\u308C\u308B\u30D1\u30BF\u30FC\u30F3\u3067\u3001\u958B\u767A\u8005\u304C\u30C6\
  \u30AD\u30B9\u30C8\u3092\u52B9\u7387\u7684\u306B\u691C\u7D22\u3001\u30DE\u30C3\u30C1\
  \u30F3\u30B0\u3001\u304A\u3088\u3073\u64CD\u4F5C\u3067\u304D\u308B\u3088\u3046\u306B\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u691C\u8A3C\
  \u3001\u89E3\u6790\u3001\u6587\u5B57\u5217\u64CD\u4F5C\u306A\u3069\u306E\u30BF\u30B9\
  \u30AF\u306B\u6B63\u898F\u8868\u73FE\u3092\u5229\u7528\u3057\u3001\u30C6\u30AD\u30B9\
  \u30C8\u51E6\u7406\u306B\u6B20\u304B\u305B\u306A\u3044\u30C4\u30FC\u30EB\u3068\u306A\
  \u3063\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.837518-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306B\u304A\u3051\u308B\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\
  \u3001\u6587\u5B57\u5217\u5185\u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\
  \u3092\u30DE\u30C3\u30C1\u30F3\u30B0\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\
  \u308C\u308B\u30D1\u30BF\u30FC\u30F3\u3067\u3001\u958B\u767A\u8005\u304C\u30C6\u30AD\
  \u30B9\u30C8\u3092\u52B9\u7387\u7684\u306B\u691C\u7D22\u3001\u30DE\u30C3\u30C1\u30F3\
  \u30B0\u3001\u304A\u3088\u3073\u64CD\u4F5C\u3067\u304D\u308B\u3088\u3046\u306B\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u691C\u8A3C\u3001\
  \u89E3\u6790\u3001\u6587\u5B57\u5217\u64CD\u4F5C\u306A\u3069\u306E\u30BF\u30B9\u30AF\
  \u306B\u6B63\u898F\u8868\u73FE\u3092\u5229\u7528\u3057\u3001\u30C6\u30AD\u30B9\u30C8\
  \u51E6\u7406\u306B\u6B20\u304B\u305B\u306A\u3044\u30C4\u30FC\u30EB\u3068\u306A\u3063\
  \u3066\u3044\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？
Rubyにおける正規表現（regex）は、文字列内の文字の組み合わせをマッチングするために使用されるパターンで、開発者がテキストを効率的に検索、マッチング、および操作できるようにします。プログラマーは、検証、解析、文字列操作などのタスクに正規表現を利用し、テキスト処理に欠かせないツールとなっています。

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
