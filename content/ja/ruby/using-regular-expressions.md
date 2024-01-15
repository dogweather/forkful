---
title:                "正規表現を使用する"
html_title:           "Ruby: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は、テキストデータ内の特定のパターンを簡単に検索したり、置換することができるためです。

## 使い方

正規表現を使用するには、Rubyの標準ライブラリである「正規表現オブジェクト」を使用します。以下のように記述します。

```Ruby
regex = /ruby/ # "ruby"というパターンの正規表現オブジェクトを作成
```

上記のように正規表現オブジェクトを作成した後、テキストデータ内にマッチする箇所を簡単に検索したり、置換したりすることができます。例えば、以下のように記述します。

```Ruby
text = "I love ruby and programming in ruby is my passion." # 検索するテキストデータ
regex = /ruby/ # "ruby"というパターンの正規表現オブジェクトを作成
matches = text.scan(regex) # テキスト内で正規表現にマッチする箇所を配列として取得
puts matches # ["ruby", "ruby"] と出力される
```

正規表現を使用することで、複雑なパターンを指定して検索することができ、効率的にテキストデータを操作することができます。

## ディープダイブ

正規表現には、多くの特殊な記号やコマンドが存在します。これらを使いこなすことで、より精度の高い検索や置換を行うことができます。

例えば、以下のようなデータがあった場合、

```Ruby
text = "My phone number is 123-456-7890 and my email is john@example.com."
```

電話番号の部分だけを抜き出すには、以下のように記述します。

```Ruby
text = "My phone number is 123-456-7890 and my email is john@example.com."
regex = /\d{3}-\d{3}-\d{4}/ # 前後に3桁の数字があり、ハイフンで区切られた箇所をマッチさせる
matches = text.scan(regex)
puts matches # ["123-456-7890"] と出力される
```

他にも、選択肢や繰り返しを指定するための記号や、マッチした箇所をグループ化するための記号など、様々な記号が用意されています。

## その他の情報

もし正規表現を使用している途中でわからなくなったり、詳しく知りたいと思ったりした場合は、以下のリンクを参考にしてください。

[正規表現チュートリアル](https://www.javadrive.jp/ruby/text/index10.html)  
[Rubyの正規表現リファレンスマニュアル](https://docs.ruby-lang.org/ja/latest/doc/regexp_rdoc.html)

## 関連リンク

[正規表現の基礎 - よく使う記号まとめ](https://qiita.com/s_yuya/items/695611f026fbfdc7a4a4)  
[Rubyで正規表現を使ってテキスト処理をする方法](https://www.javadrive.jp/ruby/regexp_basic/index1.html)