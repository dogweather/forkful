---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:26.266823-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\
  \u6587\u5B57\u5217\u306E\u30AD\u30E3\u30D4\u30BF\u30E9\u30A4\u30BA\u3068\u306F\u3001\
  \u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\
  \u5909\u63DB\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\
  \u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306B\u306F\u3001\u547D\u540D\u898F\u5247\
  \u306B\u5F93\u3046\u3001\u51FA\u529B\u3092\u8AAD\u307F\u3084\u3059\u304F\u3059\u308B\
  \u3001\u307E\u305F\u306F\u6BD4\u8F03\u3084\u683C\u7D0D\u306E\u305F\u3081\u306E\u30C7\
  \u30FC\u30BF\u306E\u4E00\u8CAB\u6027\u3092\u78BA\u4FDD\u3059\u308B\u306A\u3069\u304C\
  \u3042\u308A\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.763832-07:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\
  \u6587\u5B57\u5217\u306E\u30AD\u30E3\u30D4\u30BF\u30E9\u30A4\u30BA\u3068\u306F\u3001\
  \u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\
  \u5909\u63DB\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u3053\
  \u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\
  \u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306B\u306F\u3001\u547D\u540D\u898F\u5247\
  \u306B\u5F93\u3046\u3001\u51FA\u529B\u3092\u8AAD\u307F\u3084\u3059\u304F\u3059\u308B\
  \u3001\u307E\u305F\u306F\u6BD4\u8F03\u3084\u683C\u7D0D\u306E\u305F\u3081\u306E\u30C7\
  \u30FC\u30BF\u306E\u4E00\u8CAB\u6027\u3092\u78BA\u4FDD\u3059\u308B\u306A\u3069\u304C\
  \u3042\u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングにおいて、文字列のキャピタライズとは、文字列の最初の文字を大文字に変換し、残りを小文字にすることを指します。プログラマーがこれを行う理由には、命名規則に従う、出力を読みやすくする、または比較や格納のためのデータの一貫性を確保するなどがあります。

## どのようにして：
Rubyは文字列操作のための直感的なメソッドを提供しており、その中にはキャピタライズも含まれます。以下は、Rubyで文字列をキャピタライズする方法です：

```ruby
# Rubyの組み込みメソッド
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Rubyの`.capitalize`メソッドは便利ですが、最初の文字にのみ影響します。文字列内の各単語をキャピタライズする（タイトルケースとして知られる）ためのより多くの制御が必要な場合、または自分で実装したい場合は、RailsのActiveSupport拡張機能から`titleize`メソッドを使用するかもしれません：

```ruby
# RailsのActiveSupportの'titleize'を使用
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Railsを使用していない場合や、純粋なRubyソリューションを好む場合、以下のように文字列内の各単語をキャピタライズすることができます：

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

この方法では、文字列を単語の配列に分割し、それぞれをキャピタライズした後、スペースを挟んで再び結合します。
