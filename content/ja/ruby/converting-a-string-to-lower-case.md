---
title:                "Ruby: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 理由

Rubyプログラミングを学習する際、文字列を小文字に変換する必要がある場面があります。これは、文字列を比較する場合や特定のフォーマットに従って表示する際に便利です。

## 方法

まず、下記のように"```Ruby ... ```"というコードブロックを使用し、文字列を小文字に変換する方法を紹介します。

```Ruby
# 大文字が混ざった文字列を定義
string = "HeLLo WOrld"

# 文字列を小文字に変換
string.downcase

# 出力結果：hello world
```

上記の例では、"downcase"というメソッドを使用することで、文字列を小文字に変換することができます。

他にも、"```Ruby ... ```"コードブロック内で文字列を表示する方法もあります。

```Ruby
# 大文字が混ざった文字列を定義
string = "HeLLo WOrld"

# 文字列を小文字に変換して表示
puts string.downcase

# 出力結果：hello world
```

また、文字列を文字ごとに分割して小文字に変換することもできます。下記の例では、"chars"というメソッドを使用して文字列を分割し、"map"メソッドを使用してそれぞれの文字を小文字に変換しています。

```Ruby
# 大文字が混ざった文字列を定義
string = "HeLLo WOrld"

# 文字列を分割して小文字に変換
string.chars.map(&:downcase)

# 出力結果：["h", "e", "l", "l", "o", " ", "w", "o", "r", "l", "d"]
```

## 深堀り

Rubyにはさまざまなメソッドや関数があり、文字列を小文字に変換するための方法も多様です。例えば、"strtolower"や"lcfirst"などのメソッドを使用する方法もあります。その中で、"downcase"メソッドは文字列内で使用されるアルファベット文字をすべて小文字に変換し、”upcase”メソッドと同様に特殊文字や数字はそのままを保持します。また、"Unicode"や"ISO-8859-1"などのエンコーディング形式を指定することもできます。

## 参考リンク

- [Rubyの文字列を小文字に変換する方法](https://www.ruby-lang.org/ja/documentation/misc/refactoring/tech/tutorial/ruby-ascii8bit/)
- [downcaseメソッドの公式ドキュメント](https://docs.ruby-lang.org/ja/latest/method/String/i/downcase.html)
- [Railsチュートリアルによるdowncaseメソッドの使用例](https://railstutorial.jp/chapters/first_steps?#table:t9)
- [downcaseメソッドを使用して文字列を小文字に変換する](https://www.tutorialspoint.com/ruby/ruby_strings.htm)