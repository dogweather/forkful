---
title:                "パターンに一致する文字の削除"
html_title:           "Ruby: パターンに一致する文字の削除"
simple_title:         "パターンに一致する文字の削除"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##なぜ

なぜ誰かがパターンにマッチする文字を削除する必要があるのか、その理由を最大2文で説明します。

パターンにマッチする文字を削除することは、データの前処理や整形に役立ちます。例えば、電話番号やメールアドレスなどの特定の書式に従っていないデータを正しいフォーマットに変換する際に使用されます。

##やり方

以下は、Rubyで文字を削除する方法の例です。まずは、文字列変数を定義します。

```Ruby
str = "Ruby is a fun language!"
```

次に、正規表現を使用して文字列から特定の文字を削除します。

```Ruby
str.sub(/[aeiou]/, "") # => "Rby s a fn lngg!"
```

この場合、`sub`メソッドを使用して、文字列中の最初にマッチした文字を削除します。正規表現の`/[aeiou]/`は、母音を表しています。`"Ruby"`の`"u"`が削除されて`"Rby"`になりました。

さらに、`gsub`メソッドを使用することで、文字列中の全てのマッチした文字を削除することもできます。

```Ruby
str.gsub(/[aeiou]/, "") # => "Rby s  fn lngg!"
```

このように、パターンにマッチする文字を削除することで、データの整形が可能になります。

##掘り下げる

パターンマッチングには、正規表現や`sub`、`gsub`メソッド以外にも様々な方法があります。例えば、`delete`メソッドを使用することで、特定の文字を削除することもできます。

```Ruby
str.delete("a-z") # => "by!"
```

このように、文字を削除する方法は様々ありますが、正規表現を使用することでより柔軟にパターンにマッチする文字を削除することができます。

##関連リンク

- [Rubyの正規表現について](https://docs.ruby-lang.org/en/master/doc/ruby_manual.html#label-Regular+Expressions)
- [gsubメソッドの使い方](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub)
- [deleteメソッドの使い方](https://ruby-doc.org/core-2.7.2/String.html#method-i-delete)