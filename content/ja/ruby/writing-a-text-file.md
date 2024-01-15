---
title:                "テキストファイルの書き方"
html_title:           "Ruby: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

RMarkdownは、プログラミングのコードと文書のフォーマットを組み合わせることができます。これにより、より見やすく、管理しやすく、再利用しやすい文書を作成できます。

## How To

### テキストファイルを書く
RMarkdownでは、テキストファイルを書くために、```cat```関数を使用します。例えば、次のように入力します。

```Ruby
cat "こんにちは、日本の読者の皆さん！"
```

### フォーマットを追加する
フォーマットを追加するには、```puts```関数を使用します。例えば、次のように入力します。

```Ruby
puts "こんにちは、**日本の読者の皆さん！**"
```

### 結果
上記のコードを実行すると、次のように表示されます。

```
こんにちは、**日本の読者の皆さん！**
```

## Deep Dive

テキストファイルを作成するには、さまざまな方法があります。RMarkdownでは、```cat```関数だけでなく、```puts```関数や```print```関数も使用できます。また、フォーマットオプションも豊富で、```bold```や```italic```などのスタイルを適用することができます。

## See Also

- [Official RMarkdown Documentation](https://rmarkdown.rstudio.com/)
- [RMarkdown Tutorial](https://www.datacamp.com/community/tutorials/r-markdown-tutorial)
- [RMarkdown Cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)