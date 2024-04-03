---
date: 2024-01-26 03:41:45.369820-07:00
description: "\u3069\u3046\u3084\u308B\u304B\uFF1A Ruby\u306B\u306F\u3001\u305D\u308C\
  \u3089\u306E\u5384\u4ECB\u306A\u5F15\u7528\u7B26\u3092\u5207\u308A\u53D6\u308B\u305F\
  \u3081\u306E\u3059\u3070\u3089\u3057\u3044\u30C6\u30AF\u30CB\u30C3\u30AF\u304C\u3044\
  \u304F\u3064\u304B\u3042\u308A\u307E\u3059\u3002`gsub`\u3084`delete`\u30E1\u30BD\
  \u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3066\u4F5C\u696D\u3092\u884C\u3046\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u565B\u307F\u7815\u3044\u3066\u8003\u3048\u308B\
  \u305F\u3081\u306E\u30B3\u30FC\u30C9\u304C\u3053\u3061\u3089\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.834790-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306B\u306F\u3001\u305D\u308C\u3089\u306E\u5384\u4ECB\u306A\u5F15\u7528\
  \u7B26\u3092\u5207\u308A\u53D6\u308B\u305F\u3081\u306E\u3059\u3070\u3089\u3057\u3044\
  \u30C6\u30AF\u30CB\u30C3\u30AF\u304C\u3044\u304F\u3064\u304B\u3042\u308A\u307E\u3059\
  \u3002`gsub`\u3084`delete`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3066\
  \u4F5C\u696D\u3092\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u565B\
  \u307F\u7815\u3044\u3066\u8003\u3048\u308B\u305F\u3081\u306E\u30B3\u30FC\u30C9\u304C\
  \u3053\u3061\u3089\u3067\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## どうやるか：
Rubyには、それらの厄介な引用符を切り取るためのすばらしいテクニックがいくつかあります。`gsub`や`delete`メソッドを使用して作業を行うことができます。噛み砕いて考えるためのコードがこちらです：

```ruby
# gsubを使用して二重引用符と単引用符を削除
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# 出力: Say hello to my little friend!

# 一種類の引用符だけを扱うことがわかっている場合
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# 出力: Stay a while and listen!
```

## 深掘り
引用符の歴史は、プログラミングの最初の日々にまで遡り、それらはしばしば文字列デリミタとして機能しました。現在でも、その時と同様、不要な場合やデータの保存や操作に干渉する可能性がある場合に、これらの引用符を取り除く必要があるかもしれません。

`gsub`や`delete`について語りましたが、`tr`や`tr_s`のような他のメソッドもあり、もう少し制御をしたい場合や異なる使用例を扱うことができます：

```ruby
# trも引用符を削除することができる
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# 出力: Do or do not, there is no try.
```

これらのメソッドそれぞれが使用例を持っていることを忘れないでください。`gsub`は、複雑なパターンや複数の置換を扱う場合により強力です。`delete`と`tr`は、単純で直接的な文字の削除に美しく機能します。

## 参照
さらに読むために、そしてこれらのメソッドをより大きなコードベースで動作させることを確認するために、以下をチェックしてください：
- Rubyのドキュメントで[String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub)、[String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete)、そして[String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr)について。
- Ruby Monstasは[文字列エクササイズセット](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html)を提供しており、引用符の扱いを含みます。
- Stack Overflowの[文字列操作](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string)に関する議論は、同僚のRubyistからの現実世界の問題と解決策を提供しています。
