---
title:                "文字列から引用符を削除する"
aliases:
- /ja/ruby/removing-quotes-from-a-string/
date:                  2024-01-26T03:41:45.369820-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

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
