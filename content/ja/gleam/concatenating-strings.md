---
title:                "Gleam: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
今回は文字列を連結することで何ができるのか、その理由について紹介します。

## 方法
以下のような```Gleam```コードブロックを使って、実際に連結する方法を説明します。

```gleam
let first_name = "太郎"
let last_name = "山田"
let full_name = first_name ++" "++ last_name
```

このようにすることで、```full_name```には```太郎 山田```という文字列が格納されます。このように、```++```演算子を使うことで簡単に文字列を連結することができます。

## ディープダイブ
実は、文字列を連結する際にはいくつかの方法があります。例えば、```++```演算子以外にも、```~```演算子や```<>```演算子が存在します。それぞれの演算子の使い方や違いについて、さらに詳しく説明します。

## 参考リンク
こちらのリンクを参考にして、さらにGleamの文字列連結機能を学びましょう！
- https://gleam.run/documentation/stdlib/string.html#concatenating-strings
- https://blog.gleam.run/the-power-of-strings/ 
- https://github.com/gleam-lang/gleam/issues/632

# 他に読むべき記事
Gleamの様々な機能や使い方について詳しく解説している記事をご紹介します！

- Gleamでパズルを解く: https://blog.gleam.run/solving-puzzles-with-gleam/
- パイプラインエラー処理: https://blog.gleam.run/pipeline-error-handling-ja/
- ElixirからGleamに移行する: https://blog.gleam.run/migrating-to-gleam-from-elixir/