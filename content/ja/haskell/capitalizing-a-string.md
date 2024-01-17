---
title:                "文字列を大文字にする"
html_title:           "Haskell: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#"何 ＆ 何故？"
文字列を大文字に変換することは何かというと、プログラマーがより読みやすく効率的なコードを書くために行うことです。文字列を大文字に変換することで、コード内の特定の語句を強調することができ、より分かりやすくなります。

#"やり方："
文字列を大文字に変換するために、Haskellでは```toUpper```関数を使用します。下記のコードを参考にしてみてください。

```
-- Sample code to capitalize a string
import Data.Char

main = do
    print $ map toUpper "hello world"
```

出力： "HELLO WORLD"

#"深く潜る："
文字列を大文字に変換するというアイデアは、古くからあります。パンチカードリーダーが発明された初期の頃、大文字のみの機械で文字を読み取る必要があったため、大文字で書かれたテキストの方がより簡単に読み取ることができました。現在では、Haskell以外にも多くのプログラミング言語で同様の機能が実装されています。

#"参考："
- [Haskell Documentation on String Functions](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Wikipedia: Capitalization](https://en.wikipedia.org/wiki/Capitalization)