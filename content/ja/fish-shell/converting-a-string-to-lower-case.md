---
title:                "「文字列を小文字に変換する」"
html_title:           "Fish Shell: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換することは、プログラマーがよく行う処理の一つです。これは、文字列を検索や比較する際に、大文字と小文字を区別しないようにするためです。

## 方法：

```Fish Shell```のコードブロックで、実際のコーディングの例と出力を見ることができます。

```
# 文字列を変数に代入する
set msg "HElLo WoRLd!"

# 文字列を小文字に変換する
set msg (string tolower $msg)

# 変換後の文字列を表示する
echo $msg

# 出力結果： hello world!
```

## 深く掘り下げる：

文字列を小文字に変換することは、プログラミング言語ごとに実装方法が異なります。例えば、```bash```では```tr```コマンドを使うことができます。また、文字列を大文字に変換する機能も、多くのプログラミング言語が同じように提供しています。文字列を操作する際には、大文字と小文字を意識する必要がありますが、この処理を行うことで、より柔軟な文字列処理が可能になります。

## 参考リンク：

- [Fish Shellのドキュメント](https://fishshell.com/docs/current/)
- [文字列操作に関する記事](https://www.programiz.com/python-programming/methods/string/lower)