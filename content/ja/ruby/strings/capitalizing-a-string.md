---
title:                "文字列を大文字にする"
aliases:
- /ja/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:26.266823-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
