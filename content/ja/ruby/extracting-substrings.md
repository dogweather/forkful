---
title:                "Ruby: 文字列の抽出"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# なぜ

## 誰がこの記事を読むべきか？

この記事は、Rubyプログラミングに興味がある人や、既存のプログラムに文字列の一部を抽出する必要がある人に役立つ情報を提供します。

## 抽出されたサブストリングの使用例

```Ruby
# 文字列の一部の抽出
puts "こんにちは、私はルビーです".slice(0, 5)

# 出力結果：こんにちは

# 文字列の一部を変数として取得
name = "アヤコ"
puts "こんにちは、私は#{name}です".slice(5, 6)

# 出力結果：アヤコ

# 文字列を指定した文字数で折り返し
text = "今日はとても暑いですね。外に出るのが嫌になります。"
puts text.slice(0, 10)

# 出力結果：今日はとても暑いですね。
```

## 抽出されたサブストリングの詳細

サブストリングは、文字列の一部を取り出す方法を指します。文字列内の特定の位置から指定した文字数を取り出すことができます。Rubyの```slice```メソッドを使用すると簡単に実装できます。

```Ruby
# sliceメソッドには2つの引数があります。最初の引数は開始位置、2番目の引数は文字数を指定します。
str = "今日はとても暑いですね。外に出るのが嫌になります。"
puts str.slice(0, 10)

# 出力結果：今日はとても暑いですね。
```

サブストリングの使用例としては、特定の文字列を抽出して処理する場合や、長い文章を指定した文字数で折り返したりする場合などがあります。

## さらに詳しく学ぶ

サブストリングの詳細については、Ruby公式ドキュメントを参考にすることができます。

- [Ruby公式ドキュメント（sliceメソッド）](https://docs.ruby-lang.org/ja/latest/method/String/i/slice.html)

# もっと見る

- [Rubyプログラミング入門（サブストリングの抽出方法）](https://www.sejuku.net/blog/52721)
- [RubyAPI（sliceメソッド）](https://rubyapi.org/2.6/o/string/slice)
- [Rubyで文字列を操作する方法（サブストリングの抽出）](https://qiita.com/gomi_ningen/items/592d46eb7fb45b9a90c6)