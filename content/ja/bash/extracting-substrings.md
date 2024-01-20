---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ？
部分文字列抽出とは、元の文字列から特定の部分を取り出すことです。プログラマは情報を特定し、データを理解および操作するためにこれを行います。

## 方法:
以下はBashでの部分文字列抽出の例です。

```Bash
string="Hello, World!"
echo ${string:7:5}
```
このコードの出力は「World」となります。

## より深く:
1. 歴史的背景: Unix系OSで広く使われるBashは1989年に初版が公開されました。それ以来、部分文字列抽出はBashに搭載されるようになりました。
2. 代替案: AwkやSedなどのツールも部分文字列抽出に使用できます。しかし、Bashの短いコード構造は、部分文字列抽出を速やかに行うための優れた選択肢です。
3. 実装詳細: 上記の部分文字列抽出では、`echo ${string:7:5}` の部分は7番目の文字位置から5文字を取り出します。

## 参考資料:
1. Bashの文字列の使用法: <https://www.linuxjournal.com/content/working-strings-bash>
2. 文字列操作について理解する: <https://www.the-geek-stuff.com/2010/07/bash-string-manipulation/>
3. AwkとSedとの比較: <https://www.geekhideout.com/shell.shtml>