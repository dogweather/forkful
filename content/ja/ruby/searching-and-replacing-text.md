---
title:                "テキストの検索と置換"
html_title:           "Ruby: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換に参加する理由は、簡単に言えば、よりきれいで効率的なコードを書くためです。テキストの検索と置換は、コード内の特定のパターンを見つけて修正する素早い方法です。

## 方法

テキストの検索と置換は、Rubyの`gsub`メソッドを使用して実装することができます。例えば、`text.gsub("old", "new")`とすることで、`text`内のすべての"old"を"new"に置換することができます。

以下の例では、変数`lyrics`にある歌詞の一部を、"heart"から"love"に置換するプログラムを示します。

```Ruby
lyrics = "You'll be in my heart, yes you'll be in my heart"
lyrics.gsub("heart", "love")
#=> "You'll be in my love, yes you'll be in my love"
```

`gsub`メソッドは、すべてのマッチする部分を置換するため、一度に複数の置換を行うことができます。例えば、`lyrics.gsub("heart", "love").gsub("in", "inside")`とすることで、"heart"を"love"に、"in"を"inside"に置換することができます。

```Ruby
lyrics.gsub("heart", "love").gsub("in", "inside")
#=> "You'll be inside my love, yes you'll be inside my love"
```

## 深堀り

`gsub`メソッドは、正規表現を使用してより高度なパターンの検索と置換を行うこともできます。例えば、`lyrics.gsub(/(be.*)/, "was \1")`とすることで、"be"から始まるすべての単語を"was"に置換することができます。

```Ruby
lyrics.gsub(/(be.*)/, "was \1")
#=> "You'll was in my was, yes you'll was in my was"
```

正規表現を使用することで、より柔軟なパターンの検索と置換が可能になります。ただし、正規表現の書き方に慣れる必要がありますので、初心者の方は少し難しいかもしれません。

## 関連情報

- [gsubメソッドの公式ドキュメント (英語)](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [正規表現の基礎 (日本語)](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)
- [Rubyの正規表現チュートリアル (日本語)](https://qiita.com/jnchito/items/b9f4da4d42e1f851be5b)

## 参考文献

- [Rubyでテキスト置換をする方法 (日本語)](https://qiita.com/ryouzi/items/662c10ba8963098b0a11)
- [Rubyで正規表現を使ってテキストを置換する (日本語)](https://qiita.com/okamoto_natsuki/items/8b5c5c949862a7f474b0)
- [Rubyの基本的な正規表現の書き方 (日本語)](https://qiita.com/katsunory/items/e1fda9f70e2fd9389ebe)