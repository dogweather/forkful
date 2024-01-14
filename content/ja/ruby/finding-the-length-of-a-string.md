---
title:    "Ruby: 「文字列の長さを見つける方法」"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることに取り組む理由を説明するための1-2文です。

## 方法
文字列の長さを求める方法を観点コードブロック"```Ruby ... ```"を使用したコーディング例とサンプル出力と共に説明します。

```ruby
# 文字列の長さを求める関数
def string_length(string)
  return string.length
end

# 関数の使用例
puts string_length("こんにちは") # 5と出力される
puts string_length("") # 0と出力される
```

## 深堀り
文字列の長さを求めるために使用されるStringクラスのlengthメソッドについて、さらに詳しく説明します。このメソッドは文字列のバイト数を返すため、マルチバイト文字を使用した場合に注意が必要です。また、空の文字列の長さは0であることにも留意してください。

## 他に見る
[Best Practices for Strings in Ruby](https://www.rubyguides.com/2019/01/ruby-strings/) <br>
[String#length Documentation](https://docs.ruby-lang.org/en/2.6.0/String.html#method-i-length) <br>
[Rubyでの文字列の扱い方](https://programming-beginner-zeroichi.jp/articles/141)