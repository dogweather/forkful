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

## 検索と置換とは？

テキストを検索して特定の文字列を置き換えることを指します。プログラマーはこの作業をすることで、簡単に大量のテキストを変更することができます。

## 使い方：

```Ruby
# 文字列を検索して置換する
string = "Hello world!"
string.gsub!("world", "Ruby")
puts string
# 出力結果: Hello Ruby!

# 正規表現を使ってパターンを指定して置換する
string = "I love Ruby"
string.gsub!(/love/, "am learning")
puts string
# 出力結果: I am learning Ruby
```

## 詳細：

検索と置換の歴史は古く、コンピュータの初期から使われてきました。プログラマーが中間言語のテキストを置き換えるための他の方法もありますが、検索と置換は依然として非常に有用な方法です。Rubyでは、```gsub```メソッドを使って簡単に検索と置換を行うことができます。正規表現を使えば、より複雑なパターンの検索と置換も行うことができます。

## 関連情報：

- [Rubyドキュメント](http://ruby-doc.org/core-2.6/String.html#method-i-gsub)
- [正規表現チュートリアル](https://rubular.com/)