---
title:                "「文字列を小文字に変換する」"
html_title:           "Ruby: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する理由は、文字列の読みやすさや比較の一貫性を保つためです。また、文字列のカタカナやアクセントを変換する必要がある場合もあります。

## 方法

文字列を小文字に変換するには、Rubyメソッドの `downcase` を使用します。以下のコード例を参考にしてください。

```Ruby
str = "HELLO WORLD"
puts str.downcase
```

このコードの出力は `hello world` となります。

## ディープダイブ

Rubyの `downcase` メソッドは、文字列を小文字に変換するだけでなく、国際化や超文字の正規化にも役立つことができます。例えば、ロシア語の `Ж` を `ж` に変換できます。また、古いバージョンのRubyでは、文字列を小文字に変換する際にアクセント記号を統一するために、`String#unicode_normalize(:nfkc)` を一緒に使用する必要があります。

## 関連リンク

- [Ruby公式ドキュメント](https://ruby-doc.org/core-3.0.0/String.html#method-i-downcase)
- [小文字変換のパフォーマンス比較](https://qiita.com/jnchito/items/698625a2127e9bb55bf4)
- [Unicode正規化とは](https://wa3.i-3-i.info/word16299.html)