---
title:                "コードを関数に整理する"
aliases:
- ja/ruby/organizing-code-into-functions.md
date:                  2024-01-26T01:16:19.047883-07:00
model:                 gpt-4-0125-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 何となぜ？
コードを関数に分割することで、スクリプトを再利用可能なチャンクに分けます。これは、コードをクリーンで扱いやすく、バグの少ないものにすることについてです。モジュラーなコードは時間を節約し、精神的な余裕を保ち、デバッグやユニットテストを簡素化するため素晴らしいものです。

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
