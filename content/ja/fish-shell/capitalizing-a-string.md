---
title:                "文字列を大文字にする"
aliases:
- ja/fish-shell/capitalizing-a-string.md
date:                  2024-02-03T19:05:53.465910-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字化するとは、最初の文字を大文字にし、残りの文字列を小文字に変更することを意味します。これは、テキスト処理、ユーザー入力の正規化、特定のフォーマット基準を満たすためや一貫性を確保するためのデータフォーマッティングなど、一般的なタスクです。

## 方法:

Fish Shellでは、外部ツールやライブラリを必要とせずに、組み込み関数を使用して文字列を直接操作できます。文字列を大文字化するには、`string`コマンドとサブコマンドを組み合わせます。

```fish
# サンプル文字列
set sample_string "hello world"

# 最初の文字を大文字に
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

出力:
```
Hello world
```

文字列内の複数の単語を大文字化する必要があるシナリオ（例: "hello world" を "Hello World" に変換する）では、それぞれの単語に対して大文字化のロジックを適用しながら繰り返します：

```fish
# サンプル文
set sentence "hello fish shell programming"

# 各単語を大文字に
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# 大文字化された単語を結合
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

出力:
```
Hello Fish Shell Programming
```

Fish Shellは、いくつかのプログラミング言語がその文字列メソッドで提供するような、完全な文の大文字化を一つのコマンドで直接行う機能を提供していません。したがって、`string split`、`string sub`、`string upper`を組み合わせ、再度結合する方法は、この目的を達成するためのFish Shellにおける慣用的なアプローチを表します。
