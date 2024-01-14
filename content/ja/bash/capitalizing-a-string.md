---
title:    "Bash: 文字列の先頭を大文字にする"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングにおいて、文字列を大文字に変換する必要がある場合があります。例えば、ユーザーが入力した文字列を大文字で表示したい場合や、データベースに保存する前に文字列を正規化する必要がある場合などがあります。

## 方法
文字列を大文字に変換するには、Bashの組み込みコマンドである`tr`を使用します。以下のように記述します。

```
Bash
# 変数に文字列を格納
str="hello, world"

# 大文字に変換
str=$(echo $str | tr '[:lower:]' '[:upper:]')

# 結果を出力
echo $str
```

上記のコードを実行すると、`HELLO, WORLD`という出力が得られます。`tr`コマンドは、第一引数の文字を第二引数の文字で置き換えることができるため、大文字に変換するために使用することができます。

## 深堀り
文字列を大文字に変換する方法はいくつかありますが、`tr`コマンドは最も単純で効率的な方法です。また、`tr`コマンドは正規表現を使用することができるため、より柔軟に文字の置き換えが可能です。

例えば、`tr`コマンドを使用して数字をローマ数字に変換することもできます。以下のように記述します。

```
Bash
# 変数に数字を格納
num=123

# ローマ数字に変換
roman=$(echo $num | tr '123' 'IXV')

# 結果を出力
echo $roman
```

実行すると、`IIXV`という出力が得られます。

## その他
もしも文字列に含まれる大文字をすべて小文字に変換したい場合は、`tr`コマンドではなく`tr [:upper:] [:lower:]`を使用します。また、`tr -d`コマンドを使用すると、特定の文字を削除することもできます。

See Also
- [Bashの組み込みコマンド - tr](https://linux.die.net/man/1/tr)
- [Bashの正規表現](https://www.gnu.org/software/bash/manual/html_node/Bash-Features.html#Bash-Features)