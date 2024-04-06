---
date: 2024-01-26 01:16:19.047883-07:00
description: "\u65B9\u6CD5\uFF1A \u30E6\u30FC\u30B6\u30FC\u306B\u6328\u62F6\u3059\u308B\
  \u7C21\u5358\u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u66F8\u3044\u3066\u3044\u308B\
  \u3068\u60F3\u50CF\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T21:53:43.647921-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法：
ユーザーに挨拶する簡単なスクリプトを書いていると想像してみてください：

```Ruby
def greet(name)
  "Hello, #{name}!"
end

puts greet("Alice")   # 出力: Hello, Alice!
puts greet("Bob")     # 出力: Hello, Bob!
```

または、円の面積を計算しているかもしれません：

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # 出力: 78.53981633974483
```

よりすっきりして扱いやすいですよね？

## 深堀り
関数の概念は、Rubyではメソッドとも呼ばれますが、新しいものではなく、プログラミング自体ほど古いです。1950年代にさかのぼると、冗長性を減らすために導入されたサブルーチンがありました。

代替手段？もちろん、インラインコードがありますし、クラスとオブジェクトでOOPにすることも、ラムダやprocsで関数型にすることもできます。しかし、関数は秩序あるコードの基本です。パフォーマンスが欲しいですか？関数内のローカル変数は速く、`return`で直ちに値を返すことができます。

実装面では、`def`で関数を定義し、`end`で終えます。デフォルトパラメータを設定したり、可変長関数のためにスプラットオペレータを使用したりなどができます。関数は、あなたの望むほどシンプルまたは複雑にすることができます。

## 関連項目
- [Rubyのメソッドドキュメント](https://ruby-doc.org/core-2.7.0/Method.html)
- [Learn to Program by Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby by Sandi Metz](https://www.poodr.com/)
