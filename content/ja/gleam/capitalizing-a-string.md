---
title:    "Gleam: 文字列の大文字化"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の先頭を大文字に変換することの重要性を説明するために、最初になぜcapitalizingを行うのかについて考えてみましょう。一般的な理由としては、文字列をより見やすくしたり、データの整形や正規化を行ったりするために使用することが挙げられます。

## 方法

GLEAMでは、文字列を大文字に変換するための便利な関数が用意されています。以下のコード例を参考に、どのように文字列を大文字に変換するかを学んでみましょう。

```Gleam
let sentence = "hello world"
let capitalized = String.capitalize(sentence)
# 出力: "Hello world"
```

上記の例では、まず変数`sentence`に文字列"hello world"を代入します。次に、`String.capitalize`関数を使用して、`sentence`を大文字に変換し、`capitalized`という新しい変数に代入しました。出力を確認すると、文字列の先頭が大文字になっていることが分かります。

また、GLEAMでは文字列の一部だけを大文字に変換することもできます。例えば、次のようにすることで文字列の2文字目を大文字に変換できます。

```Gleam
let sentence = "hello world"
let partial = String.partial_capitalize(sentence, 1..1)
# 出力: "hEllo world"
```

## ディープダイブ

`String.capitalize`関数は内部でパターンマッチングを使用することで、より複雑な文字列の大文字変換がある文字列にも対応できるようになっています。また、`String.partial_capitalize`関数も同様にパターンマッチングを利用しています。

さらに、GLEAMではUnicodeのサポートも含まれているため、英語以外の文字に対しても同じように大文字変換を行うことができます。

## 参考リンク

- GLEAMドキュメント (https://gleam.run/documentation/)
- プログラミング言語GLEAMの基本 (https://qiita.com/hirokidaichi/items/e5c04fbad2ad531c3ca9)
- Unicodeのサポートについて (https://gleam.run/documentation/introduction/text.html#unicode)