---
title:    "Ruby: テキストの検索と置換"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##なぜ

プログラマーにとって、テキストの検索と置換は非常に重要なスキルです。これにより、コードや文章をより迅速かつ正確に編集することができます。

##使い方

検索と置換はRubyで非常に簡単に行うことができます。以下のコードブロックを参考にしてください。

```Ruby
# 文章内の特定の単語を別の単語に置換する例
text = "こんにちは、私はRubyを勉強しています。"
puts text.gsub("Ruby", "プログラミング")

# 出力: こんにちは、私はプログラミングを勉強しています。
```

```Ruby
# 正規表現を使用して文中の文字列を置換する例
text = "今日はとても暑いです。"
puts text.gsub(/\S+/, "涼しい")

# 出力: 涼しい涼しい涼しい。
```

##詳細情報

Rubyの文法を理解することで、検索と置換のパターンを自由に作成することができます。また、正規表現を使用することで、より複雑な検索と置換を行うことも可能です。これらの機能を活用することで、より強力なテキスト編集ツールとしてRubyを活用することができます。

##参考リンク

- [Rubyドキュメント](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [正規表現チュートリアル](https://rubular.com/)
- [プログラミング学習サイト「ドットインストール」の正規表現レッスン](https://dotinstall.com/lessons/basic_regexp)

##参考になるもの

- [Rubyの標準入力から特定の文字列を置換する方法](https://www.narakeet.com/ruby/string-input-gsub)
- [Rubyで文字列置換を行う方法](https://www.ipentec.com/document/ruby-replace-string-match-content-with-regex)
- [Rubyで正規表現を使用して文字列を置換する方法](https://www.atmarkit.co.jp/ait/articles/1805/04/news021.html)