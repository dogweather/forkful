---
title:                "Gleam: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#【なぜ】
文字列を小文字に変換することについての理由を1-2文で説明します。

文字列を小文字に変換することは、プログラマーにとって非常に便利な機能です。例えば、ユーザーからの入力を正規化するためや、大文字小文字を区別せずに文字列を比較するためなど、さまざまなシナリオで利用されます。

#【方法】
以下の「```Gleam ...```」コードブロックを参考に、文字列を小文字に変換する方法を学んでみましょう。

```Gleam
fn main() {
    let str = "Hello World";
    let lower = str.to_lower_case();
    io.println(lower);
}
```

上記のコードを実行すると、`hello world`という出力が得られます。

#【深堀り】
文字列を小文字に変換するには、Gleamに組み込まれた`to_lower_case()`関数を使用します。この関数は、文字列を小文字に変換して、新しい文字列を返します。元の文字列そのものを変更するわけではありません。

また、Gleamでは日本語のようなマルチバイト文字もサポートされています。そのため、日本語の文字列を小文字に変換することも可能です。

#【See Also】
- [Gleam公式ドキュメント](https://gleam.run/)
- [Gleamのマルチバイト文字のサポートについて](https://gleam.run/documentation#multibyte-strings)

以上で文字列を小文字に変換する方法を学びました。Gleamに慣れることで、より効率的なプログラミングができるようになるでしょう。ぜひ、Gleamを活用してみてください。