---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列を大文字にするとは、小文字だったアルファベットを全部または一部を大文字に変換することです。これは、タイトルや固有名詞を強調したり、一貫したフォーマットを維持するために行われます。

## How to (やり方)
Bashでの文字列の大文字化は、`tr` コマンドや組み込みの機能を使って行えます。ここに二つの例を示します。

```Bash
# 全ての文字を大文字にする
echo "hello world" | tr '[:lower:]' '[:upper:]'
```
出力:
```
HELLO WORLD
```

```Bash
# 変数を使った文字列の最初の文字だけを大文字にする
str="hello world"
echo "${str^}"
```
出力:
```
Hello world
```

## Deep Dive (掘り下げ)
UNIXやLinuxの`tr`コマンドは、文字の置換や削除に使われる古くからあるコマンドです。これで簡単に文字列を大文字や小文字に変換できます。しかし、Bash 4.0以降では、組み込み機能が追加され、特別なパターンを使って文字列の変換を行なうことができるようになりました。例えば、`${str^^}`を使って全ての文字を大文字に、`${str,}`で最初の文字だけを小文字に変更できます。

他の言語には専用の関数がありますが、Bashはシンプルな構造のため、コマンドやパターンに頼ります。

## See Also (関連情報)
- Bashのマニュアル: [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html)
- tr コマンドの情報: [tr man page](https://man7.org/linux/man-pages/man1/tr.1.html)
- 組み込みパターンと文字列操作: [Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
