---
title:    "Ruby: 文字列の連結"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ？

文字列を連結するとは、複数の文字列を組み合わせて1つの文字列にすることです。これができると、より複雑な文字列や文を作成することができます。

## 方法

```Ruby
first_name = "Satoshi"
last_name = "Tanaka"

full_name = first_name + " " + last_name
puts full_name
```

この例では、`+`演算子を使用して`first_name`、スペース、`last_name`を連結することで、`full_name`を作成しています。コードを実行すると、全体の名前が`Satoshi Tanaka`と出力されます。

## 深堀り

Rubyでは、他にも文字列を連結する方法があります。例えば、`<<`演算子を使用することもできます。

```Ruby
first_name = "Satoshi"
last_name = "Tanaka"

full_name = first_name << " " << last_name
puts full_name
```

このコードを実行しても同じ出力結果が得られますが、`<<`演算子はより効率的に文字列を連結することができます。また、`+=`演算子を使用すれば、元の変数を更新しながら文字列を連結することもできます。

```Ruby
first_name = "Satoshi"
last_name = "Tanaka"

first_name += " "
first_name += last_name
puts first_name
```

また、文字列内に変数を含めることもできます。

```Ruby
name = "Satoshi"

puts "こんにちは、私の名前は#{name}です。"
```

このように、文字列を連結する方法はさまざまですが、どの方法を選択しても同じ結果を得ることができます。

## 詳しくは

Rubyでは、文字列の連結についてさらに詳しく学ぶことができます。例えば、文字列を配列に変換してから連結する方法や、特殊文字を使用して文字列を整形する方法などがあります。究極的には、自分にとって最も理解しやすい方法で文字列を連結することが重要です。

## 参考リンク

- [Rubyの文字列操作](https://www.techscore.com/blog/2016/12/21/ruby%E3%81%AE%E6%96%87%E5%AD%97%E5%88%97%E6%93%8D%E4%BD%9C/)
- [Ruby Wing](https://rubywing.com/string/)
- [Rubyリファレンスマニュアル](https://docs.ruby-lang.org/ja/latest/doc/spec=2fcall=3f.html#gseq2697)