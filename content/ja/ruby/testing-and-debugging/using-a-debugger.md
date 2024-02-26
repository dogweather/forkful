---
date: 2024-01-26 04:09:54.104643-07:00
description: "\u2026"
lastmod: '2024-02-25T18:49:40.793981-07:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？

Rubyでデバッガーを使用することは、プログラマーにコードを一時停止させ、変数を調査し、コードを1行ずつ進めるというスーパーパワーを与えます。バグを潰すため、コードフローを理解するため、そして書かれた呪文（コード）が魔法が起こるとき—または起こらないとき—に正確に何をしているのかを見るために、人々はそれを行います。

## 方法：

Rubyには`byebug`と呼ばれる組み込みのデバッガーがあります。まず、Gemfileに`byebug`を含めて`bundle install`を実行します。次に、プログラムを一時停止させたい場所に`byebug`を置きます。

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

このスクリプトを実行すると、`byebug`で実行が一時停止し、コマンドを入力できるインタラクティブセッションに投げ込まれます。例えば：

```
step
next
continue
var local
```

サンプル出力では、このようなプロンプトが表示されます：

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## 深掘り：

`byebug`よりも前には、Rubyistたちは`debugger`と`pry`を使用していました。後者の`pry`は、デバッグに`binding.pry`ブレークポイントを使用できる強力なREPLであり、デバッガー以上のものです。

Rubyの`byebug`に代わるものには、`pry`と`byebug`の機能を組み合わせた`pry-byebug`や、アクティブにメンテナンスされていない古いgemである`ruby-debug`があります。

`byebug`を呼び出すと、デバッガーはコード実行を一時停止し、実行時に目を通すことを可能にします。変数を見たり変更したり、コードの異なるポイントにジャンプしたり、さらにはRubyのコードを1行ずつ実行したりできます。これは、Rubyコードのためのタイムトラベル能力を持っているようなものです。

## 参照：

- Byebug GitHubリポジトリ: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pryドキュメント: [https://github.com/pry/pry](https://github.com/pry/pry)
- Railsアプリのデバッグガイド: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
