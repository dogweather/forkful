---
date: 2024-01-26 03:41:45.369820-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.834790-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u5265\u304C\u3059\
  \u3068\u3044\u3046\u306E\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u5024\u306E\u5468\u308A\
  \u3092\u5305\u3093\u3067\u3044\u308B\u305D\u306E\u30C0\u30D6\u30EB\u307E\u305F\u306F\
  \u30B7\u30F3\u30B0\u30EB\u306E\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\u304F\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\
  \u30C3\u30D7\u3059\u308B\u305F\u3081\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u306E\u4E00\
  \u8CAB\u6027\u3092\u78BA\u4FDD\u3059\u308B\u305F\u3081\u3001\u3042\u308B\u3044\u306F\
  \u4F59\u5206\u306A\u6587\u5B57\u306B\u3088\u3063\u3066\u6DF7\u4E71\u3059\u308B\u53EF\
  \u80FD\u6027\u306E\u3042\u308B\u30B7\u30B9\u30C6\u30E0\u306E\u305F\u3081\u306B\u30C7\
  \u30FC\u30BF\u3092\u6E96\u5099\u3059\u308B\u305F\u3081\u306B\u3001\u3057\u3070\u3057\
  \u3070\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 何となぜ？
文字列から引用符を剥がすというのは、テキスト値の周りを包んでいるそのダブルまたはシングルの引用符を取り除くことを意味します。プログラマーは、ユーザー入力をクリーンアップするため、データ処理の一貫性を確保するため、あるいは余分な文字によって混乱する可能性のあるシステムのためにデータを準備するために、しばしばこれを行います。

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
