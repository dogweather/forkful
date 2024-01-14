---
title:                "Fish Shell: 文字列の大文字化"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ？
文字列の最初の文字を大文字に変換することは、多くのプログラミング言語で便利な機能です。例えば、フォームの入力欄で入力された名前を正しい形式で表示するために使用することができます。

## 方法
```Fish Shell```のコマンドを使用して、文字列を大文字に変換する方法を学びましょう。下の例を参考にしてください。

```
# 大文字に変換される文字列を入力
set name "sakura"

# 最初の文字を大文字に変換
echo (string upper -s $name)

# 出力結果：Sakura
```

## 深堀り
上記の例では、```string upper```コマンドを使用して文字列を大文字に変換しましたが、実際には、このコマンドは文字列に含まれるすべての単語の最初の文字を大文字に変換することができます。また、特定の文字列のみを大文字に変換することも可能です。

```
# 文字列のみを大文字に変換
echo (string upper -s "hello world")

# 出力結果：HELLO WORLD

# 特定の文字列のみを大文字に変換
echo (string escape -s "hello world" H)

# 出力結果：HhHhH HhHhH
```

## 併せて読む
Markdownヘッダー：## 併せて読む
- [Fish Shell公式サイト](https://fishshell.com/)
- [現場で使えるシェルプログラミング入門](https://www.amazon.co.jp/dp/4839967436)