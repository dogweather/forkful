---
title:                "文字列を連結する"
html_title:           "Ruby: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ？

文字列を連結することの最大のメリットは、複数の変数やデータを一つの文字列にまとめることができる点です。これにより、プログラムのコードがスッキリとし、処理速度も向上します。

## 方法

```Ruby
# 文字列を連結する方法
first_name = "太郎"
last_name = "山田"
full_name = first_name + " " + last_name

puts full_name
#=> 太郎 山田
```

```Ruby
# 数値と文字列を連結する方法
age = 23
puts "年齢は" + age.to_s + "歳です。"
#=> 年齢は23歳です。
```

```Ruby
# 文字列の中に変数を入れる方法
greeting = "こんにちは、私の名前は#{full_name}です。"
puts greeting
#=> こんにちは、私の名前は太郎 山田です。
```

```Ruby
# 文字列の中に式を入れる方法
price = 1000
tax_rate = 0.1
total_price = "料金は#{price * (1 + tax_rate)}円です。"
puts total_price
#=> 料金は1100円です。
```

## 深堀り

文字列を連結する際に使用する`+`演算子は、Rubyにおいては`String`クラスの`concat`メソッドとして定義されています。また、`<<`演算子も同様に`concat`メソッドとして機能します。どちらも元の文字列を変更するため、注意が必要です。

## See Also

- [RubyのStringクラス](https://docs.ruby-lang.org/ja/latest/class/String.html)
- [Stringクラスのconcatメソッド](https://docs.ruby-lang.org/ja/latest/method/String/i/concatMethod.html)